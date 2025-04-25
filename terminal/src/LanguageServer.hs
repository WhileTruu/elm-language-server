{-# LANGUAGE OverloadedStrings #-}

module LanguageServer
  ( run
  )
  where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.MVar
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Time

import Data.Foldable (foldrM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import qualified Data.Utf8 as Utf8
import qualified Data.Set as Set

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
import qualified Elm.Outline as Outline
import qualified Elm.Version as Version
import qualified Elm.Package as Pkg
import qualified Deps.Registry


import qualified AST.Source as Src
import qualified Elm.ModuleName as ModuleName

import qualified BackgroundWriter as BW

newtype State = State
  { _changedFiles :: Control.Concurrent.MVar.MVar (Map.Map FilePath String)
  }


run :: IO ()
run = do
  state <- State <$> Control.Concurrent.MVar.newMVar Map.empty

  loop state

  where
    loop state =
      do  contentLength <- readHeader
          body <- BSLC.hGet IO.stdin (contentLength + 2)

          case Aeson.parseEither (\obj -> obj .: "method") =<< Aeson.eitherDecode body of
            Left err ->
              do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                  IO.hFlush IO.stderr
                  loop state

            Right "initialized" ->
              do  loop state

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
                          loop state

                    Right (id, rootPath) ->
                      do  let response =
                                Aeson.object
                                  [ "capabilities" .= Aeson.object
                                    [ "definitionProvider" .= Aeson.object []
                                    -- , "documentSymbolProvider" .= True
                                    , "textDocumentSync" .= Aeson.object
                                         [ "openClose" .= True
                                         , "change" .= (2 :: Int)
                                         ]
                                    , "referencesProvider" .= Aeson.object
                                       [ "workDoneProgress" .= True
                                       ]
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
                          sendProgressEnd "initialization-progress" "Done"

                          loop state

            Right "textDocument/didOpen" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              textDocument <- params .: "textDocument"
                              version <- textDocument .: "version" :: Aeson.Parser Int

                              uri <- textDocument .: "uri" :: Aeson.Parser String
                              let filePath :: FilePath
                                  filePath = drop 7 uri

                              text <- textDocument .: "text" :: Aeson.Parser String

                              return (version, filePath, text)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right (version, filePath, text) ->
                      do  let mVar = _changedFiles state

                          Control.Concurrent.MVar.modifyMVar_ mVar $ \a ->
                            return $ Map.insert filePath text a

                          loop state

            Right "textDocument/didClose" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              textDocument <- params .: "textDocument"

                              uri <- textDocument .: "uri" :: Aeson.Parser String
                              let filePath :: FilePath
                                  filePath = drop 7 uri

                              return filePath
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right filePath ->
                      do  let mVar = _changedFiles state

                          Control.Concurrent.MVar.modifyMVar_ mVar $ \a ->
                            return $ Map.delete filePath a

                          loop state

            Right "textDocument/didChange" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              textDocument <- params .: "textDocument"
                              version <- textDocument .: "version" :: Aeson.Parser Int
                              uri <- textDocument .: "uri" :: Aeson.Parser String
                              let filePath :: FilePath
                                  filePath = drop 7 uri

                              changes <- mapM parseTextDocumentContentChangeEvent =<< params .: "contentChanges" :: Aeson.Parser [((A.Position, A.Position), String)]

                              return (version, filePath, changes)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right (version, filePath, changes) ->
                       do  let mVar = _changedFiles state
                           files <- Control.Concurrent.MVar.takeMVar mVar
                           let updatedFiles = Map.adjust (applyChanges changes) filePath files
                           Control.Concurrent.MVar.putMVar mVar updatedFiles

                           loop state

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
                          loop state

                    Right (id, filePath, position) ->
                      do  sendCreateWorkDoneProgress "go-to-definition-progress"
                          sendProgressBegin "go-to-definition-progress" "👀 Finding definition"

                          startTime <- Data.Time.getCurrentTime
                          result <- Task.run $ findDefinition state filePath position
                          endTime <- Data.Time.getCurrentTime

                          sendProgressEnd "go-to-definition-progress" $
                             "Done in " ++ show (Data.Time.diffUTCTime endTime startTime)

                          case result of
                            Right (definitionFilePath, _, A.At region _) ->
                              do  respond id $ encodeRegion definitionFilePath region
                                  loop state

                            Left err ->
                              do  respondErr id $ Reporting.Exit.toString $
                                    definitionExitToReport filePath err
                                  loop state

            Right "textDocument/references" ->
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
                          loop state

                    Right (id, filePath, position) ->
                      do  -- FIXME: use provided work done token?
                          sendCreateWorkDoneProgress "find-references"
                          sendProgressBegin "find-references" "🔍 Finding references"

                          startTime <- Data.Time.getCurrentTime
                          result <- Task.run $ findReferences state filePath position
                          endTime <- Data.Time.getCurrentTime
                          let timeDiff = Data.Time.diffUTCTime endTime startTime

                          case result of
                            Right references ->
                              do  sendProgressEnd "find-references" $
                                    "Found " ++ show (length references) ++ " in " ++ show timeDiff

                                  respond id $ Aeson.toJSON $ map (uncurry encodeRegion) references
                                  loop state

                            Left err ->
                              do  sendProgressEnd "find-references" $
                                    "Failed " ++ show (length result) ++ " in " ++ show timeDiff

                                  respondErr id $ Reporting.Exit.toString $
                                    definitionExitToReport filePath err
                                  loop state

            Right unknownMethod ->
              do  IO.hPutStr IO.stderr ("Unknown method: " ++ unknownMethod)
                  IO.hFlush IO.stderr
                  loop state


parsePosition :: Aeson.Value -> Aeson.Parser A.Position
parsePosition =
  Aeson.withObject "Position" $ \position ->
    do  row <- position .: "line" :: Aeson.Parser Int
        column <- position .: "character" :: Aeson.Parser Int

        return $ A.Position (fromIntegral row + 1) (fromIntegral column + 1)


parseRange :: Aeson.Value -> Aeson.Parser (A.Position, A.Position)
parseRange =
  Aeson.withObject "Range" $ \obj ->
    do  start <- parsePosition =<< obj .: "start"
        end <- parsePosition =<< obj .: "end"

        return (start, end)


parseTextDocumentContentChangeEvent :: Aeson.Value -> Aeson.Parser ((A.Position, A.Position), String)
parseTextDocumentContentChangeEvent =
  Aeson.withObject "TextDocumentContentChangeEvent" $ \obj ->
    do  range <- parseRange =<< obj .: "range"
        text <- obj .: "text"

        return (range, text)


applyChanges :: [((A.Position, A.Position), String)] -> String -> String
applyChanges changes content =
  List.foldl' (\acc ((start, end), newText) -> applyChange acc start end newText) content changes


applyChange :: String -> A.Position -> A.Position -> String -> String
applyChange content (A.Position sr sc) (A.Position er ec) newText =
  let lines_ = lines content
      (before, rest) = splitAt (fromIntegral sr - 1) lines_
      (startTargetLine:afterStart) = rest

      endRest = drop (fromIntegral er - fromIntegral sr) rest
      (endTargetLine:afterEnd) = endRest

      (start, _) = splitAt (fromIntegral sc - 1) startTargetLine
      (_, end) = splitAt (fromIntegral ec - 1) endTargetLine

      updatedLine = start ++ newText ++ end
  in unlines $ before ++ (updatedLine : afterEnd)


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
        ]
      ]
    )


sendProgressReport :: String -> String -> IO ()
sendProgressReport token message = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("report" :: String)
        , "message" Aeson..= message
        ]
      ]
    )


sendProgressEnd :: String -> String -> IO ()
sendProgressEnd token message = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("end" :: String)
        , "message" Aeson..= message
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


respondErr :: Int -> String -> IO ()
respondErr idValue message =
  let
    header = "Content-Length: " ++ show (BSC.length content) ++ "\r\n\r\n"
    content = BSLC.toStrict $ Aeson.encode $ Aeson.object
      [ "id" Aeson..= idValue
      , "error" Aeson..= Aeson.object
        [ "code" Aeson..= (-1 :: Int) -- FIXME: remove code?
        , "message" Aeson..= (message :: String)
        ]
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
  | DefinitionExitModuleNotFound ModuleName.Raw
  | DefinitionExitNoProperModName DefinedEntity


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
      -- FIXME: Add info about where looked for the definition in?
      Reporting.Exit.Help.report "NO DEFINITION" Nothing
        ("I tried to find the definition for " ++ definedEntityToStr entity ++ ", but failed to find it.")
        []

    DefinitionExitNoDefinedEntity ->
      Reporting.Exit.Help.report "NO DEFINED ENTITY UNDER CURSOR" Nothing
        "I tried to find a defined entity under the cursor, but could not."
        []

    DefinitionExitModuleNotFound moduleName ->
      Reporting.Exit.Help.report "NO FILE FOR MODULE" Nothing
        ("I tried to find the file for " ++ ModuleName.toChars moduleName ++ ", but failed to find it.")
        []

    DefinitionExitNoProperModName entity ->
      Reporting.Exit.Help.report "NO PROPER MODULE NAME" Nothing
        ("I tried to find the definition for " ++ definedEntityToStr entity ++ ", but failed to find it.")
        []


type Found = A.Located Found_


data Found_
  = FoundValue Src.Value
  | FoundPattern Src.Pattern_
  | FoundDef Src.Def


findDefinition ::
  State
  -> FilePath
  -> A.Position
  -> Task.Task DefinitionExit (FilePath, ModuleName.Raw, Found)
findDefinition state filePath position =
  Task.eio id $ BW.withScope $ \scope -> Task.run $
  do  maybeRoot <- Task.io $ Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Task.throw DefinitionExitNoRoot

        Just root ->
          Task.eio id $ Stuff.withRootLock root $ Task.run $

          do  details <-
                Task.eio DefinitionExitBadDetails $ Details.load Reporting.silent scope root

              findDefinition_ state details filePath position


findDefinition_ ::
  State
  -> Details.Details
  -> FilePath
  -> A.Position
  -> Task.Task DefinitionExit (FilePath, ModuleName.Raw, Found)
findDefinition_ state details filePath position =
    do  files <- Task.io $ Control.Concurrent.MVar.readMVar (_changedFiles state)

        source <-
          maybe (Task.io $ File.readUtf8 filePath) return $
            (fmap BSC.pack $ Map.lookup filePath files)

        let projectType =
              case Details._outline details of
                Details.ValidApp _ -> Parse.Application
                Details.ValidPkg pkgName _ _ -> Parse.Package pkgName

        srcModule@(Src.Module _ _ _ imports_ _ _ _ _ _) <-
          Task.eio (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
            return (Parse.fromByteString projectType source)

        entity <- maybe (Task.throw DefinitionExitNoDefinedEntity) return $
          findDefinedEntityInValues position srcModule

        let row = ((\(A.Position row _) -> row) position)

        case entity of
          DEVar defs patterns Src.LowVar name ->
            do  let local = fmap (\a -> (filePath, Src.getName srcModule, a)) $
                              findDefinitionForLowVarLocally srcModule defs patterns name
                external <- findDefinitionForLowVarInImports state details imports_ name

                maybe (Task.throw $ DefinitionExitNotFound entity) return $
                  local <|> fmap (\(a, b, c) -> (a, b, fmap FoundValue c)) external

          DEVarQual _ _ Src.LowVar mod name ->
            do  imported <- findDefinitionForLowVarQualInImports state details imports_ mod name

                maybe (Task.throw $ DefinitionExitNotFound entity) return $
                  fmap (\(a, b, c) -> (a, b, fmap FoundValue c)) imported

          _ ->
            Task.throw $ DefinitionExitNotFound entity


findDefinitionForLowVarLocally :: Src.Module -> [A.Located Src.Def] -> [Src.Pattern] -> Name -> Maybe Found
findDefinitionForLowVarLocally (Src.Module _ _ _ _ values _ _ _ _) defs patterns name =
  let
    inDefs =
      foldr
        (\(A.At _ def) acc ->
          case def of
            (Src.Define (A.At region valueName) _ _ _) ->
              if valueName == name then Just (A.At region (FoundDef def)) else acc
            (Src.Destruct pattern _) ->
              fmap (\(a, b) -> A.At a (FoundPattern b)) (findDefinitionForNameInPattern name pattern)
        )
        Nothing
        defs

    inPatterns =
      foldr
        (\p acc ->
          fmap (\(a, b) -> A.At a (FoundPattern b))
            (findDefinitionForNameInPattern name p) <|> acc
        )
        Nothing
        patterns

    inValues =
      foldr
        (\(A.At _ value@(Src.Value (A.At region valueName) _ _ _)) acc ->
          if valueName == name then Just (A.At region (FoundValue value)) else acc
        )
        Nothing
        values
  in
  inDefs <|> inPatterns <|> inValues


findDefinitionForLowVarQualInImports ::
  State
  -> Details.Details
  -> [Src.Import]
  -> Name
  -> Name
  -> Task.Task DefinitionExit (Maybe (FilePath, ModuleName.Raw, A.Located Src.Value))
findDefinitionForLowVarQualInImports state details imports qual name =
  let
    potentialSources =
      foldr
        (\import_@(Src.Import iName iAlias iExposing) acc ->
          if A.toValue iName == qual || Just qual == iAlias then import_ : acc else acc
        )
        []
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details (Src.getImportName import_) name
          y <- acc
          return (x <|> y)
    )
    (return Nothing)
    potentialSources


findDefinitionForLowVarInImports ::
  State
  -> Details.Details
  -> [Src.Import]
  -> Name
  -> Task.Task DefinitionExit (Maybe (FilePath, ModuleName.Raw, A.Located Src.Value))
findDefinitionForLowVarInImports state details imports name =
  let
    potentialSources =
      foldr
        (\import_@(Src.Import iName iAlias iExposing) acc ->
          case iExposing of
            Src.Open -> import_ : acc
            Src.Explicit exposed -> foldr (\exposed _ ->
                case exposed of
                  Src.Lower (A.At _ name_) -> if name_ == name then [import_] else acc
                  Src.Upper _ _ -> acc
                  Src.Operator _ _ -> acc
              )
              []
              exposed
        )
        []
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details (Src.getImportName import_) name
          y <- acc
          return (x <|> y)
    )
    (return Nothing)
    potentialSources


findDefinitionForNameInModule ::
  State
  -> Details.Details
  -> ModuleName.Raw
  -> Name
  -> Task.Task DefinitionExit (Maybe (FilePath, ModuleName.Raw, A.Located Src.Value))
findDefinitionForNameInModule state details moduleName name =
  do  files <- Task.io $ Control.Concurrent.MVar.readMVar (_changedFiles state)

      (projectType, filePath) <-
        Task.mio (DefinitionExitModuleNotFound moduleName) $
          (do  let local = fmap (\a -> (Parse.Application, a)) (lookupModulePath details moduleName)
               pkg <- fmap (\a -> (,) <$> fmap Parse.Package (lookupPkgName details moduleName) <*> a)
                  (lookupPkgPath details moduleName)

               return (local <|> pkg)
          )

      source <-
        maybe (Task.io $ File.readUtf8 filePath) return $
          (BSC.pack <$> Map.lookup filePath files)

      -- Check if file contains the val at all before parsing, which is more
      -- expensive if the file is massive.
      -- TODO: do not parse for definition either? :D
      let sourceContainsVal = any (BSC.isPrefixOf (BSC.pack (Name.toChars name ++ " "))) (BSC.lines source)

      if sourceContainsVal then
        do  srcModule <-
              Task.eio (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                return (Parse.fromByteString projectType source)

            return (fmap (\a -> (filePath, moduleName, a)) $ findLowVarDefinitionNamed name srcModule)
      else
        return Nothing

lookupPkgPath :: Details.Details -> ModuleName.Raw -> IO (Maybe FilePath)
lookupPkgPath details moduleName =
  case lookupPkgName details moduleName of
    Nothing -> pure Nothing
    Just pkgName ->
      do  maybeCurrentVersion <- getPackageCurrentlyUsedOrLatestVersion "." pkgName

          case maybeCurrentVersion of
            Nothing -> pure Nothing
            Just version ->
              do  packageCache <- Stuff.getPackageCache
                  let home = Stuff.package packageCache pkgName version
                  let path = home Path.</> "src" Path.</> ModuleName.toFilePath moduleName Path.<.>"elm"

                  return (Just path)


lookupModulePath :: Details.Details -> ModuleName.Raw -> Maybe FilePath
lookupModulePath details name =
  fmap Details._path $ Map.lookup name $ Details._locals details


lookupPkgName :: Details.Details -> ModuleName.Raw -> Maybe Pkg.Name
lookupPkgName details canModuleName =
  fmap (\(Details.Foreign name_ _) -> name_) $
    Map.lookup canModuleName $
    Details._foreigns details


findLowVarDefinitionNamed :: Name -> Src.Module -> Maybe (A.Located Src.Value)
findLowVarDefinitionNamed name (Src.Module _ _ _ _ values _ _ _ _) =
  foldr (\value@(A.At _ (Src.Value name_ _ _ _)) acc ->
      if A.toValue name_ == name then Just value else acc
    )
    Nothing
    values


findDefinitionForNameInPattern :: Name -> Src.Pattern -> Maybe (A.Region, Src.Pattern_)
findDefinitionForNameInPattern name pattern@(A.At _ pattern_) =
  case pattern_ of
    Src.PVar pname ->
      if pname == name then Just (A.toRegion pattern, A.toValue pattern) else Nothing
    Src.PRecord names ->
      foldr (\(A.At loc name_) acc -> if name_ == name then Just (loc, pattern_) else acc) Nothing names
    Src.PAlias aPattern (A.At loc aName) ->
      if aName == name then
        Just (loc, pattern_)
      else
        findDefinitionForNameInPattern name aPattern
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
    DEVar _ _ _ name -> Name.toChars name
    DEVarQual _ _ _ prefix name -> Name.toChars prefix ++ "." ++ Name.toChars name
    DEAccess _ _ record field -> "." ++ Name.toChars field


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



-- REFERENCES


findReferences :: State -> FilePath -> A.Position -> Task.Task DefinitionExit [(FilePath, A.Region)]
findReferences state filePath position =
  Task.eio id $ BW.withScope $ \scope -> Task.run $
  do  maybeRoot <- Task.io $ Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Task.throw DefinitionExitNoRoot

        Just root ->
          Task.eio id $ Stuff.withRootLock root $ Task.run $

          do  details <-
                Task.eio DefinitionExitBadDetails $ Details.load Reporting.silent scope root

              definition <- findDefinition_ state details filePath position

              case definition of
                (modulePath, moduleName, A.At defRegion (FoundValue value@(Src.Value name _ _ _))) ->
                  do  let importers = importersOf details moduleName

                      files <- Task.io $ Control.Concurrent.MVar.readMVar (_changedFiles state)

                      source <-
                        maybe (Task.io $ File.readUtf8 modulePath) return $
                          (fmap BSC.pack $ Map.lookup modulePath files)

                      let projectType =
                            case Details._outline details of
                              Details.ValidApp _ -> Parse.Application
                              Details.ValidPkg pkgName _ _ -> Parse.Package pkgName

                      srcModule@(Src.Module _ _ _ imports_ _ _ _ _ _) <-
                        Task.eio (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                          return (Parse.fromByteString projectType source)

                      let localRefs = map (\a -> (modulePath, a))
                            (varInModule (A.toValue name) srcModule)

                      foldr
                        (\a acc ->
                          do  path <- Task.mio (DefinitionExitModuleNotFound a) $
                                        return (lookupModulePath details a)

                              source <-
                                maybe (Task.io $ File.readUtf8 path) return $
                                  (fmap BSC.pack $ Map.lookup path files)

                              let projectType =
                                    case Details._outline details of
                                      Details.ValidApp _ -> Parse.Application
                                      Details.ValidPkg pkgName _ _ -> Parse.Package pkgName

                              srcModule@(Src.Module _ _ _ imports_ _ _ _ _ _) <-
                                Task.eio (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                                  return (Parse.fromByteString projectType source)

                              foundRefs <- acc

                              let newRefs = case List.find (\(Src.Import name_ _ _) -> A.toValue name_ == moduleName) imports_ of
                                              Just (import_@(Src.Import _ alias _)) ->
                                                if isExposed import_ (A.toValue name)
                                                  then varInModule (A.toValue name) srcModule
                                                  else varQualInModule (Maybe.fromMaybe (Src.getImportName import_) alias) (A.toValue name) srcModule

                                              Nothing -> []

                              return (foundRefs ++ map (\a -> (path, a)) newRefs)
                        )
                        (return localRefs)
                        (Set.toList importers)

                _ -> return []

isExposed :: Src.Import -> Name -> Bool
isExposed (Src.Import _ _ Src.Open) name = True
isExposed (Src.Import _ _ (Src.Explicit exposing)) name =
  any
    (\exposing_ ->
      case exposing_ of
        Src.Lower (A.At _ name_) -> name == name_
        Src.Upper _ _ -> False
        Src.Operator _ _ -> False
    )
    exposing

importersOf :: Details.Details -> ModuleName.Raw -> Set.Set ModuleName.Raw
importersOf details targetModule =
  let locals = Details._locals details
  in
  Map.foldrWithKey
    (\localModuleName localDetails found ->
      if List.elem targetModule (Details._deps localDetails)
        then Set.insert localModuleName found
        else found
    )
    Set.empty
    locals


varInModule :: Name -> Src.Module -> [A.Region]
varInModule name srcMod@(Src.Module _ _ _ imports values _ _ _ _) =
    List.concatMap
      (\(A.At _ (Src.Value _ _ expr _)) ->
        varInExpr name [] expr
      )
      values


varInExpr :: Name -> [A.Region] -> Src.Expr -> [A.Region]
varInExpr name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ varName -> if varName == name then region : foundRegions else foundRegions
        Src.VarQual _ qual varName -> foundRegions
        Src.List exprs -> List.foldl (varInExpr name) foundRegions exprs
        Src.Op opName ->
          -- FIXME: should op be here?
          if opName == name then region : foundRegions else foundRegions
        Src.Negate expr -> varInExpr name foundRegions expr

        Src.Binops exprsAndNames expr ->
            List.foldl
                (\foundRegions (expr_, _) -> varInExpr name foundRegions expr_)
                (varInExpr name foundRegions expr)
                exprsAndNames

        Src.Lambda patterns expr -> varInExpr name foundRegions expr
        Src.Call expr exprs ->
            List.foldl
                (varInExpr name)
                (varInExpr name foundRegions expr)
                exprs

        Src.If listTupleExprs expr ->
            List.foldl
                (\foundRegions (one, two) ->
                    varInExpr name (varInExpr name foundRegions one) two
                )
                (varInExpr name foundRegions expr)
                listTupleExprs

        Src.Let defs expr ->
            List.foldl
                (\foundRegions (A.At _ def_) ->
                    case def_ of
                        Src.Define (A.At _ name_) _ expr_ _ ->
                            varInExpr name foundRegions expr_

                        Src.Destruct pattern expr_ ->
                            varInExpr name foundRegions expr_
                )
                (varInExpr name foundRegions expr)
                defs

        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    varInExpr name (varInExpr name foundRegions branchExpr) expr
                )
                (varInExpr name foundRegions expr)
                branches

        Src.Accessor _ -> foundRegions
        Src.Access expr _ -> varInExpr name foundRegions expr

        Src.Update _ fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> varInExpr name foundRegions fieldExpr)
                foundRegions
                fields

        Src.Record fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> varInExpr name foundRegions fieldExpr)
                foundRegions
                fields

        Src.Unit -> foundRegions
        Src.Tuple exprA exprB exprs ->
            List.foldl (varInExpr name)
                foundRegions
                (exprA : exprB : exprs)
        Src.Shader _ _ -> foundRegions


varQualInModule :: Name -> Name -> Src.Module -> [A.Region]
varQualInModule qual name srcMod@(Src.Module _ _ _ _ values _ _ _ _) =
    List.concatMap
      (\(A.At _ (Src.Value _ _ expr _)) ->
        varQualInExpr qual name [] expr
      )
      values


varQualInExpr :: Name -> Name -> [A.Region] -> Src.Expr -> [A.Region]
varQualInExpr qual name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ varName -> foundRegions
        Src.VarQual _ qual varName ->
          if qual == qual && varName == name then region : foundRegions else foundRegions
        Src.List exprs -> List.foldl (varQualInExpr qual name) foundRegions exprs
        Src.Op opName -> foundRegions
        Src.Negate expr -> varQualInExpr qual name foundRegions expr
        Src.Binops exprsAndNames expr ->
            List.foldl
                (\foundRegions (expr_, _) -> varQualInExpr qual name foundRegions expr_)
                (varQualInExpr qual name foundRegions expr)
                exprsAndNames

        Src.Lambda patterns expr -> varQualInExpr qual name foundRegions expr
        Src.Call expr exprs ->
            List.foldl
                (varQualInExpr qual name)
                (varQualInExpr qual name foundRegions expr)
                exprs

        Src.If listTupleExprs expr ->
            List.foldl
                (\foundRegions (one, two) ->
                    varQualInExpr qual name (varQualInExpr qual name foundRegions one) two
                )
                (varQualInExpr qual name foundRegions expr)
                listTupleExprs

        Src.Let defs expr ->
            List.foldl
                (\foundRegions (A.At _ def_) ->
                    case def_ of
                        Src.Define (A.At _ name_) _ expr_ _ ->
                            varQualInExpr qual name foundRegions expr_

                        Src.Destruct pattern expr_ ->
                            varQualInExpr qual name foundRegions expr_
                )
                (varInExpr name foundRegions expr)
                defs

        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    varQualInExpr qual name (varQualInExpr qual name foundRegions branchExpr) expr
                )
                (varQualInExpr qual name foundRegions expr)
                branches

        Src.Accessor _ -> foundRegions
        Src.Access expr _ -> varQualInExpr qual name foundRegions expr

        Src.Update _ fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> varQualInExpr qual name foundRegions fieldExpr)
                foundRegions
                fields

        Src.Record fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> varQualInExpr qual name foundRegions fieldExpr)
                foundRegions
                fields

        Src.Unit -> foundRegions
        Src.Tuple exprA exprB exprs ->
            List.foldl (varQualInExpr qual name) foundRegions (exprA : exprB : exprs)
        Src.Shader _ _ -> foundRegions


-- PACKAGE


getPackageCurrentlyUsedOrLatestVersion :: FilePath -> Pkg.Name -> IO (Maybe Version.Version)
getPackageCurrentlyUsedOrLatestVersion rootDir packageName =
  do  eitherOutline <- Outline.read rootDir
      case eitherOutline of
        Left err -> getPackageNewestVersionFromRegistry packageName

        Right (Outline.App appOutline) ->
          let maybeLocal =
                Map.lookup packageName (Outline._app_deps_direct appOutline)
                  <|> Map.lookup packageName (Outline._app_deps_indirect appOutline)
                  <|> Map.lookup packageName (Outline._app_test_direct appOutline)
                  <|> Map.lookup packageName (Outline._app_test_indirect appOutline)
          in
          case maybeLocal of
            Nothing -> getPackageNewestVersionFromRegistry packageName
            Just found -> pure maybeLocal

        Right (Outline.Pkg _) ->
          getPackageNewestVersionFromRegistry packageName


getPackageNewestVersionFromRegistry packageName =
  do  packageCache <- Stuff.getPackageCache
      maybeRegistry <- Deps.Registry.read packageCache

      case maybeRegistry of
        Nothing ->
          pure Nothing

        Just registry ->
          case Map.lookup packageName (Deps.Registry._versions registry) of
            Nothing -> pure Nothing
            Just knownVersions -> pure (Just (Deps.Registry._newest knownVersions))
