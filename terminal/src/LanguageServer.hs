{-# LANGUAGE OverloadedStrings #-}

module LanguageServer
  ( run
  )
  where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Name (Name)
import qualified Data.Name as Name

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified File
import qualified System.FilePath as Path

import qualified Stuff

import qualified Parse.Module as Parse

import qualified Reporting
import qualified Reporting.Doc
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit
import qualified Reporting.Exit.Help
import qualified Reporting.Report
import qualified Reporting.Render.Code as Code
import qualified Reporting.Task as Task
import qualified Reporting.Annotation as A

import qualified Elm.Details as Details
import qualified Elm.Outline
import qualified BackgroundWriter as BW

import qualified AST.Source as Src

run :: IO ()
run = do
  loop ()

  where
    loop () =
      do  contentLength <- readHeader
          body <- BSLC.hGet IO.stdin (contentLength + 2)

          case Aeson.parseEither (\obj -> obj .: "method") =<< Aeson.eitherDecode body of
            Left err ->
              do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                  IO.hFlush IO.stderr
                  loop ()

            Right "initialized" ->
              do  loop ()

            Right "initialize" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id"
                              -- FIXME: type annotation needed because value is not used probably
                              rootPath <- params .: "rootPath" :: Aeson.Parser String
                              return ( id, rootPath )
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                          IO.hFlush IO.stderr
                          loop ()

                    Right (id, rootPath) ->
                      do  let response =
                                Aeson.object
                                  [ "capabilities" .= Aeson.object
                                    [ "definitionProvider" .= Aeson.object []
                                    -- , "documentSymbolProvider" .= True
                                    -- , "textDocumentSync" .= Aeson.object
                                    --     [ "save" .= True
                                    --     , "openClose" .= True
                                    --     ]
                                    -- , "referencesProvider" .= Aeson.object
                                    --   [ "workDoneProgress" .= True
                                    --   ]
                                    -- , "hoverProvider" .= Aeson.object
                                    --   [ "workDoneProgress" .= True
                                    --   ]
                                    ]
                                  , "serverInfo" .= Aeson.object
                                    [ "name" .= ("whiletruu-elm-language-server" :: String)
                                    , "version" .= ("0.0.1" :: String)
                                    ]
                                  ]

                          respond id response

                          sendCreateWorkDoneProgress "initialization-progress"
                          sendProgressBegin "initialization-progress" "Discovering projects"
                          sendProgressEnd "initialization-progress"

                          loop ()

            Right "textDocument/definition" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id" :: Aeson.Parser Int

                              textDocument <- params .: "textDocument"
                              uri <- textDocument .: "uri" :: Aeson.Parser String

                              position <- params .: "position"
                              row <- position .: "line" :: Aeson.Parser Int
                              column <- position .: "character" :: Aeson.Parser Int

                              let filePath = drop 7 uri
                              let position = A.Position
                                               (fromIntegral row + 1)
                                               (fromIntegral column + 1)

                              return (id, filePath, position)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop ()

                    Right (id, filePath, position) ->
                      do  result <- Task.run $ findDefinition filePath position

                          case result of
                            Right (definitionFilePath, region) ->
                              do  respond id $ encodeRegion filePath region
                                  loop ()

                            Left err ->
                              do  showMessage MessageTypeError $
                                    Reporting.Exit.toString $
                                    definitionExitToReport filePath err

                                  IO.hSetBuffering IO.stderr IO.NoBuffering
                                  Reporting.Exit.toStderr $ definitionExitToReport filePath err
                                  IO.hFlush IO.stderr
                                  IO.hSetBuffering IO.stderr (IO.BlockBuffering Nothing)
                                  loop ()

            Right unknownMethod ->
              do  IO.hPutStr IO.stderr ("Unknown method: " ++ unknownMethod)
                  IO.hFlush IO.stderr
                  loop ()





readHeader :: IO Int
readHeader = do
  line <- BSC.hGetLine IO.stdin
  if "Content-Length: " `BSC.isPrefixOf` line
    then return (read $ BSC.unpack $ BSC.drop 16 line)
    else readHeader


respond :: Int -> Aeson.Value -> IO ()
respond idValue value =
  let
    header = "Content-Length: " ++ show (BSC.length content) ++ "\r\n\r\n"
    content = BSLC.toStrict $ Aeson.encode $ Aeson.object
      [ "id" .= idValue
      , "result" .= value
      ]
   in do
   BSC.hPutStr IO.stdout (BSC.pack header `BSC.append` content)
   IO.hFlush IO.stdout


sendCreateWorkDoneProgress :: String -> IO ()
sendCreateWorkDoneProgress token = do
  sendNotification "window/workDoneProgress/create"
    (Aeson.object
      [ "token" Aeson..= token
      ]
    )


sendProgressBegin :: String -> String -> IO ()
sendProgressBegin token title = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("begin" :: String)
        , "title" Aeson..= title
        -- , "message" Aeson..= ("YOLO" :: String)
        ]
      ]
    )


sendProgressEnd :: String -> IO ()
sendProgressEnd token = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("end" :: String)
        ]
      ]
    )


sendNotification :: String -> Aeson.Value -> IO ()
sendNotification method value =
  let
    header = "Content-Length: " ++ show (BSC.length content) ++ "\r\n\r\n"
    content = BSLC.toStrict $ Aeson.encode $ Aeson.object
      [ "method" .= method
      , "params" .= value
      ]
   in do
   BSC.hPutStr IO.stdout (BSC.pack header `BSC.append` content)
   IO.hFlush IO.stdout


encodeRegion :: FilePath -> A.Region -> Aeson.Value
encodeRegion filePath (A.Region (A.Position sr sc) (A.Position er ec)) =
  Aeson.object
    [ "uri" .= ("file://" ++ filePath :: String)
    , "range" .= Aeson.object
        [ "start" .= Aeson.object
            [ "line" .= (sr - 1)
            , "character" .= (sc - 1)
            ]
        , "end" .= Aeson.object
            [ "line" .= (er - 1)
            , "character" .= (ec - 1)
            ]
        ]
    ]


showMessage :: MessageType -> String -> IO ()
showMessage messageType message =
  sendNotification "window/showMessage"
    (Aeson.object
      [ "type" Aeson..= messageTypeToValue messageType
      , "message" Aeson..= message
      ]
    )


data MessageType
  = MessageTypeError
  | MessageTypeWarning
  | MessageTypeInfo
  | MessageTypeLog
  | MessageTypeDebug
  deriving (Show)


messageTypeToValue :: MessageType -> Int
messageTypeToValue messageType =
  case messageType of
    MessageTypeError -> 1
    MessageTypeWarning -> 2
    MessageTypeInfo -> 3
    MessageTypeLog -> 4
    MessageTypeDebug -> 5



-- DEFINITION


data DefinitionExit
  = DefinitionExitBadDetails Reporting.Exit.Details
  | DefinitionExitBadInput BS.ByteString Reporting.Error.Error
  | DefinitionExitNoRoot
  | DefinitionExitNotFound DefinedEntity
  | DefinitionExitNoDefinedEntity

definitionExitToReport :: FilePath -> DefinitionExit -> Reporting.Exit.Help.Report
definitionExitToReport path exit =
  case exit of
    DefinitionExitBadDetails details ->
      Reporting.Exit.toDetailsReport details

    DefinitionExitBadInput source error ->
      Reporting.Exit.Help.compilerReport "/" (Reporting.Error.Module "???" path File.zeroTime source error) []

    DefinitionExitNoRoot ->
      Reporting.Exit.Help.report "DEFINITION FOR WHAT?" Nothing
        "I cannot find an elm.json so I am not sure where you want me to find things from."
        [ Reporting.Doc.reflow $
            "Elm packages always have an elm.json that says current the version number. If\
            \ you run this command from a directory with an elm.json file, I will try to bump\
            \ the version in there based on the API changes."
        ]

    DefinitionExitNotFound entity ->
      Reporting.Exit.Help.report "NO DEFINITION" Nothing
        ("I tried to find the definition for " ++ definedEntityToStr entity ++ ", but failed to find it.")
        []

    DefinitionExitNoDefinedEntity ->
      Reporting.Exit.Help.report "NO DEFINED ENTITY UNDER CURSOR" Nothing
        "I tried to find a defined entity under the cursor, but could not."
        []


findDefinition :: FilePath -> A.Position -> Task.Task DefinitionExit (FilePath, A.Region)
findDefinition filePath position =
  Task.eio id $ BW.withScope $ \scope -> Task.run $
  do  maybeRoot <- Task.io $ Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Task.throw DefinitionExitNoRoot

        Just root ->
          Task.eio id $ Stuff.withRootLock root $ Task.run $

          do  Task.io (IO.hPutStr IO.stderr $ "Root: " ++ root)
              Task.io (IO.hFlush IO.stderr)

              details <-
                Task.eio DefinitionExitBadDetails $ Details.load Reporting.silent scope root

              source <-
                Task.io $ File.readUtf8 filePath

              srcModule <-
                Task.eio (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                  return (Parse.fromByteString Parse.Application source)

              definedEntity <- maybe (Task.throw DefinitionExitNoDefinedEntity) return $
                findDefinedEntityInValues position srcModule

              region <- maybe (Task.throw $ DefinitionExitNotFound definedEntity) return $
                findDefinitionForDefinedEntity srcModule definedEntity

              pure (filePath, region)


findDefinitionForDefinedEntity :: Src.Module -> DefinedEntity -> Maybe A.Region
findDefinitionForDefinedEntity (Src.Module name exports docs imports values unions alias infixes effects) definedEntity =
  case definedEntity of
    DEVar defs patterns Src.LowVar name ->
      let
        inDefs =
          foldr
            (\(A.At _ def) acc ->
              case def of
                (Src.Define (A.At region valueName) _ _ _) -> if valueName == name then Just region else acc
                (Src.Destruct pattern _) -> findDefinitionForNameInPattern name pattern
            )
            Nothing
            defs

        inPatterns =
          foldr (\p acc -> findDefinitionForNameInPattern name p <|> acc) Nothing patterns

        inValues =
          foldr
            (\(A.At _ (Src.Value (A.At region valueName) _ _ _)) acc ->
              if valueName == name then Just region else acc
            )
            Nothing
            values
      in
      inDefs <|> inPatterns <|> inValues

    _ ->
      Nothing


findDefinitionForNameInPattern :: Name -> Src.Pattern -> Maybe A.Region
findDefinitionForNameInPattern name pattern@(A.At _ pattern_) =
  case pattern_ of
    Src.PVar pname ->
      if pname == name then Just (A.toRegion pattern) else Nothing
    Src.PRecord names -> foldr (\name_ acc ->
      if A.toValue name_ == name then Just (A.toRegion name_) else acc) Nothing names
    Src.PAlias aPattern (A.At loc aName) ->
      if aName == name then Just loc else findDefinitionForNameInPattern name aPattern
    Src.PTuple a b c ->
      foldr (\p acc -> findDefinitionForNameInPattern  name p <|> acc) Nothing (a : b : c)
    Src.PList patterns ->
      foldr (\p acc -> findDefinitionForNameInPattern name p <|> acc) Nothing patterns
    Src.PCons a b ->
      findDefinitionForNameInPattern name a <|> findDefinitionForNameInPattern name b
    Src.PCtor _ _ args ->
      foldr (\p acc -> findDefinitionForNameInPattern name p <|> acc) Nothing args
    _ ->
      Nothing


data DefinedEntity
  = DEVar [A.Located Src.Def] [Src.Pattern] Src.VarType Name
  | DEVarQual [A.Located Src.Def] [Src.Pattern] Src.VarType Name Name
  | DEAccess [A.Located Src.Def] [Src.Pattern] Src.Expr Name


definedEntityToStr :: DefinedEntity -> String
definedEntityToStr entity =
  case entity of
    DEVar _ _ _ name -> "var: " ++ Name.toChars name
    DEVarQual _ _ _ prefix name -> "qualified var: " ++ Name.toChars prefix ++ " " ++ Name.toChars name
    DEAccess _ _ record field -> "access: " ++ Name.toChars field


findDefinedEntityInValues :: A.Position -> Src.Module -> Maybe DefinedEntity
findDefinedEntityInValues position (Src.Module name exports docs imports values unions alias infixes effects) =
  foldr
    (\located found ->
      case located of
        A.At region (Src.Value _ patterns body _) ->
          if isInRegion position region
            then findDefinedEntityInExpr position [] patterns (A.toValue body)
            else found
    )
    Nothing
    values


isInRegion :: A.Position -> A.Region -> Bool
isInRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
    && (row == endRow && col <= endCol || row < endRow)


findDefinedEntityInExpr
  :: A.Position
  -> [A.Located Src.Def]
  -> [Src.Pattern]
  -> Src.Expr_
  -> Maybe DefinedEntity
findDefinedEntityInExpr position defs patterns expr =
  case expr of
    Src.Chr _ ->
      Nothing

    Src.Str _ ->
      Nothing

    Src.Int _ ->
      Nothing

    Src.Float _ ->
      Nothing

    Src.Var varType name ->
      Just $ DEVar defs patterns varType name

    Src.VarQual varType prefix name ->
      Just $ DEVarQual defs patterns varType prefix name

    Src.List exprs ->
      foldr
        (\a acc ->
          if isInRegion position (A.toRegion a) then
            findDefinedEntityInExpr position defs patterns (A.toValue a)
          else
            acc
        )
        Nothing
        exprs

    Src.Op _ ->
      Nothing

    Src.Negate expr ->
      if isInRegion position (A.toRegion expr) then
        findDefinedEntityInExpr position defs patterns (A.toValue expr)
      else
        Nothing

    Src.Binops ops final ->
      if isInRegion position (A.toRegion final) then
        findDefinedEntityInExpr position defs patterns (A.toValue final)
      else
        foldr
          (\a acc ->
            if isInRegion position (A.toRegion (fst a)) then
              findDefinedEntityInExpr position defs patterns (A.toValue (fst a))
            else
              acc
          )
          Nothing
          ops

    Src.Lambda srcArgs body ->
      if isInRegion position (A.toRegion body) then
        findDefinedEntityInExpr position defs (srcArgs ++ patterns) (A.toValue body)
      else
        Nothing

    Src.Call func args ->
      if isInRegion position (A.toRegion func) then
        findDefinedEntityInExpr position defs patterns (A.toValue func)
      else
        foldr
          (\a acc ->
            if isInRegion position (A.toRegion a) then
              findDefinedEntityInExpr position defs patterns (A.toValue a)
            else
              acc
          )
          Nothing
          args

    Src.If branches finally ->
      if isInRegion position (A.toRegion finally) then
        findDefinedEntityInExpr position defs patterns (A.toValue finally)
      else
        foldr
          (\(condition, branch) acc ->
            if isInRegion position (A.toRegion condition) then
              findDefinedEntityInExpr position defs patterns (A.toValue condition)
            else if isInRegion position (A.toRegion branch) then
              findDefinedEntityInExpr position defs patterns (A.toValue branch)
            else
              acc
          )
          Nothing
          branches

    Src.Let defs1 body ->
      if isInRegion position (A.toRegion body) then
        findDefinedEntityInExpr position (defs1 ++ defs) patterns (A.toValue body)
      else
        foldr
          (\def acc ->
              case (A.toValue def) of
                Src.Define _ patterns1 expr _ ->
                  if isInRegion position (A.toRegion expr) then
                    findDefinedEntityInExpr position (def : defs) (patterns1 ++ patterns) (A.toValue expr)

                  else
                    acc

                Src.Destruct pattern expr ->
                  if isInRegion position (A.toRegion expr) then
                    findDefinedEntityInExpr position (def : defs) (pattern : patterns) (A.toValue expr)
                  else
                    acc
          )
          Nothing
          defs1

    Src.Case expr branches ->
      if isInRegion position (A.toRegion expr) then
        findDefinedEntityInExpr position defs patterns (A.toValue expr)
      else
        foldr
          (\(pattern, branch) acc ->
            if isInRegion position (A.toRegion branch) then
              findDefinedEntityInExpr position defs (pattern : patterns) (A.toValue branch)
            else
              acc
          )
          Nothing
          branches

    Src.Accessor field ->
      Nothing

    Src.Access record field ->
      if isInRegion position (A.toRegion field) then
        Just $ DEAccess defs patterns record (A.toValue field)
      else if isInRegion position (A.toRegion record) then
        findDefinedEntityInExpr position defs patterns (A.toValue record)
      else
        Nothing

    Src.Update starter fields ->
      if isInRegion position (A.toRegion starter) then
        Just $ DEVar defs patterns Src.LowVar (A.toValue starter)

      else
        foldr
          (\(field, value) acc ->
            if isInRegion position (A.toRegion field) then
              Just $ DEVar defs patterns Src.LowVar (A.toValue field)
            else if isInRegion position (A.toRegion value) then
              findDefinedEntityInExpr position defs patterns (A.toValue value)
            else
              acc
          )
          Nothing
          fields

    Src.Record fields ->
      foldr
        (\(field, value) acc ->
          if isInRegion position (A.toRegion value) then
            findDefinedEntityInExpr position defs patterns (A.toValue value)
          else
            acc
        )
        Nothing
        fields

    Src.Unit ->
      Nothing

    Src.Tuple a b cs ->
      foldr
        (\expr exprs ->
          if isInRegion position (A.toRegion expr) then
            findDefinedEntityInExpr position defs patterns (A.toValue expr)
          else
            exprs
        )
        Nothing
        (a : b : cs)

    Src.Shader _ _ ->
      Nothing

