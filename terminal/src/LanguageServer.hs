{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LanguageServer
  ( run
  )
  where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.Time
import qualified Data.Bifunctor
import qualified Data.Functor
import qualified Debug.Trace
import Data.Foldable (foldrM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.NonEmptyList
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Map
import qualified Data.Either
import qualified Data.ByteString.UTF8 as BS_UTF8

import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Proc

import qualified File
import qualified System.FilePath as Path
import System.FilePath ((</>), (<.>))

import qualified Stuff
import qualified Build
import qualified Compile

import qualified Parse.Module as Parse
import qualified Parse.Variable as Parse

import qualified Reporting
import qualified Reporting.Doc
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit
import qualified Reporting.Warning
import qualified Reporting.Exit.Help
import qualified Reporting.Report
import qualified Reporting.Render.Code as Code
import qualified Reporting.Task as Task
import qualified Reporting.Annotation as A
import qualified Reporting.Result
import qualified Reporting.Error.Type
import qualified Reporting.Error.Docs
import qualified Reporting.Render.Type.Localizer

import qualified Elm.Details as Details
import qualified Elm.Outline as Outline
import qualified Elm.Version as Version
import qualified Elm.Interface as Interface
import qualified Elm.Docs as Docs
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Compiler.Type as Type

import qualified Elm.Package as Pkg
import qualified Deps.Registry
import qualified Nitpick.PatternMatches
import qualified Optimize.Module

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Canonicalize.Module
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName

import qualified BackgroundWriter as BW

import qualified Type.Constrain.Module as Type
import qualified System.IO.Unsafe
import qualified Type.Solve as Type

import qualified Json.String
import qualified Control.Monad

import Data.Function ((&))

import qualified Http

import LanguageServer.Reporting as LsReporting


data State =
  State
    { _changedFiles :: Control.Concurrent.MVar.MVar (Map.Map FilePath BS.ByteString)
    , _prevPublishedDiagnosticsFiles :: Control.Concurrent.MVar.MVar [FilePath]
    , _elmFormat :: Control.Concurrent.MVar.MVar (Maybe FilePath)
    }


run :: IO ()
run =
  do  changedFiles <- Control.Concurrent.MVar.newMVar Map.empty
      prevPublishedDiagnosticsFiles <- Control.Concurrent.MVar.newMVar []
      elmFormat <- Control.Concurrent.MVar.newMVar Nothing
      runLoop $ State changedFiles prevPublishedDiagnosticsFiles elmFormat


runLoop :: State -> IO ()
runLoop state =
  do  contentLength <- readHeader
      body <- BSLC.hGet IO.stdin (contentLength + 2)

      case Aeson.parseEither (\a -> a .: "method") =<< Aeson.eitherDecode body of
        Left err ->
          do  putStrFlushErr $ "Error decoding JSON: " ++ err
              runLoop state

        Right method ->
          do  handleMessage state method body
              runLoop state


handleMessage :: State -> [Char] -> BSLC.ByteString ->  IO ()
handleMessage state method body =
  case method of
    "initialize" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id"
                      rootPath <- params .: "rootPath" :: Aeson.Parser String
                      initializationOptions <- params Aeson..:? "initializationOptions" :: Aeson.Parser (Maybe Aeson.Object)

                      languageServer <- maybe (pure Nothing) (\a -> a Aeson..:? "whiletruu-elm-language-server") initializationOptions
                      elmFormatPath <- maybe (pure Nothing) (\a -> a Aeson..:? "elmFormatPath") languageServer

                      return ( requestID, rootPath, elmFormatPath)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, rootPath, elmFormatPath) ->
              do  elmFormat <- getElmFormat elmFormatPath

                  Control.Concurrent.MVar.modifyMVar_ (_elmFormat state) $
                    \_ -> pure elmFormat

                  let response =
                        Aeson.object
                          [ "capabilities" .= Aeson.object
                            [ "definitionProvider" .= True
                            , "documentSymbolProvider" .= True
                            , "documentFormattingProvider" .= Maybe.isJust elmFormat
                            , "renameProvider" .= Aeson.object
                               [ "prepareProvider" .= True
                               ]
                            , "textDocumentSync" .= Aeson.object
                                 [ "openClose" .= True
                                 , "change" .= (2 :: Int)
                                 -- FIXME: use includeText
                                 -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave
                                 , "save" .= True
                                 ]
                            , "referencesProvider" .= True
                            ]
                          , "serverInfo" .= Aeson.object
                            [ "name" .= ("whiletruu-elm-language-server" :: String)
                            , "version" .= ("1.1.0" :: String)
                            ]
                          ]
                  respond requestID response

    "initialized" ->
      return ()

    "shutdown" ->
      do  let result = Aeson.parseEither (\obj -> obj .: "id") =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right requestID ->
              respond requestID Aeson.Null

    "exit" ->
      Exit.exitSuccess

    "textDocument/didOpen" ->
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
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (version, filePath, text) ->
              Control.Concurrent.MVar.modifyMVar_ (_changedFiles state) $ \a ->
                return $ Map.insert filePath (BS_UTF8.fromString text) a

    "textDocument/didSave" ->
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
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right filePath ->
              do  let mVar = _changedFiles state

                  style <- Reporting.languageServer
                  diagnosticsResult <- diagnostics style state filePath

                  case diagnosticsResult of
                    Left err ->
                      showMessage MessageTypeError $ Reporting.Exit.toString $
                        diagnosticsExitToReport err

                    Right stuffs ->
                      do  prev <- Control.Concurrent.MVar.readMVar (_prevPublishedDiagnosticsFiles state)

                          let diff = List.filter (\a -> List.all (\(n, _, _) -> n /= a) stuffs) prev

                          mapM_ (\a -> publishReportDiagnostic a 1 []) diff

                          Control.Concurrent.MVar.modifyMVar_ (_prevPublishedDiagnosticsFiles state)
                            (\_ -> pure (map (\(a,_,_) -> a) stuffs))

                          mapM_
                            (\(reportsFilePath, i, reports) ->
                              publishReportDiagnostic reportsFilePath i reports
                            )
                            stuffs

    "textDocument/didClose" ->
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
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right filePath ->
              do  let mVar = _changedFiles state

                  Control.Concurrent.MVar.modifyMVar_ mVar $ \a ->
                    return $ Map.delete filePath a

    "textDocument/didChange" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      textDocument <- params .: "textDocument"
                      version <- textDocument .: "version" :: Aeson.Parser Int
                      uri <- textDocument .: "uri" :: Aeson.Parser String
                      let filePath :: FilePath
                          filePath = drop 7 uri

                      changes <-
                        mapM parseTextDocumentContentChangeEvent =<<
                          params .: "contentChanges" :: Aeson.Parser [((A.Position, A.Position), String)]

                      return (version, filePath, changes)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (version, filePath, changes) ->
               do  let mVar = _changedFiles state
                   files <- Control.Concurrent.MVar.takeMVar mVar
                   let updatedFiles = Map.adjust (applyChanges changes) filePath files
                   Control.Concurrent.MVar.putMVar mVar updatedFiles

    "textDocument/definition" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      position <- params .: "position"
                      row <- position .: "line" :: Aeson.Parser Int
                      column <- position .: "character" :: Aeson.Parser Int

                      let filePath = drop 7 uri
                      let pos = A.Position (fromIntegral row + 1) (fromIntegral column + 1)

                      return (requestID, filePath, pos)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath, position) ->
              do  style <- Reporting.languageServer
                  definitionResult <- findDefinition style state filePath position

                  case definitionResult of
                    Right (definitionFilePath, _, _, A.At region _) ->
                      respond requestID $ encodeRegion definitionFilePath region

                    Left err ->
                      respondErr requestID $ Reporting.Exit.toString $
                        definitionExitToReport filePath err

    "textDocument/references" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      position <- params .: "position"
                      row <- position .: "line" :: Aeson.Parser Int
                      column <- position .: "character" :: Aeson.Parser Int

                      let filePath = drop 7 uri
                      let pos = A.Position
                                       (fromIntegral row + 1)
                                       (fromIntegral column + 1)

                      return (requestID, filePath, pos)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath, position) ->
              do  style <- Reporting.languageServer
                  referencesResult <- findReferences style state filePath position

                  case referencesResult of
                    Right references ->
                      respond requestID
                        $ Aeson.toJSON
                        $ concatMap (\(fp, regions) ->
                            map (encodeRegion fp) regions
                          ) (Map.toList references)

                    Left err ->
                      respondErr requestID $ Reporting.Exit.toString $
                        definitionExitToReport filePath err

    "textDocument/prepareRename" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      position <- params .: "position"
                      row <- position .: "line" :: Aeson.Parser Int
                      column <- position .: "character" :: Aeson.Parser Int

                      let filePath = drop 7 uri
                      let pos = A.Position
                                       (fromIntegral row + 1)
                                       (fromIntegral column + 1)

                      return (requestID, filePath, pos)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath, position) ->
              do  style <- Reporting.languageServer
                  maybeTarget <- findDefinition style state filePath position
                  case maybeTarget of
                    Right (_, _, element, _) ->
                      do  respond requestID $
                            Aeson.object
                              [ "range" .= encodeRange (A.toRegion element)
                              , "placeholder" .=  (elementToRenamePlaceholder element :: String)
                              ]

                    Left _ ->
                      respond requestID Aeson.Null

    "textDocument/rename" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      position <- params .: "position"
                      row <- position .: "line" :: Aeson.Parser Int
                      column <- position .: "character" :: Aeson.Parser Int
                      newName <- params .: "newName" :: Aeson.Parser String

                      let filePath = drop 7 uri
                      let pos = A.Position
                                       (fromIntegral row + 1)
                                       (fromIntegral column + 1)

                      return (requestID, filePath, pos, newName)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath, pos, newName) ->
              do  style <- Reporting.languageServer
                  referencesResult <- findReferences style state filePath pos

                  case referencesResult of
                    Right references ->
                      do  let amount = length (concat (Map.elems references))
                          respond requestID $ Aeson.toJSON $
                             encodeWorkspaceEdit references (Name.fromChars newName)

                    Left err ->
                      do  respondErr requestID $ Reporting.Exit.toString $
                            definitionExitToReport filePath err

    "textDocument/formatting" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      let filePath = drop 7 uri

                      pure (requestID, filePath)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath) ->
              do  elmFormat <- Control.Concurrent.MVar.readMVar (_elmFormat state)

                  case elmFormat of
                    Nothing ->
                      do  respondErr requestID "elm-format not found"

                    Just executable ->
                      do  files <- Control.Concurrent.MVar.readMVar (_changedFiles state)
                          source <- maybe (File.readUtf8 filePath) return $ Map.lookup filePath files

                          formattingResult <-
                            Exception.try $
                              Proc.readCreateProcessWithExitCode (Proc.proc executable ["--stdin"]) (BS_UTF8.toString source)
                              :: IO (Either IOError (Exit.ExitCode, String, String))

                          case formattingResult of
                            Left _ ->
                              respondErr requestID $ "Failed to run elm-format: " ++ executable

                            Right (Exit.ExitFailure _, _, stderr_ )->
                              respondErr requestID stderr_

                            Right (Exit.ExitSuccess, stdout_, _)->
                              let srcLines = BSC.split '\n' source in
                              respond requestID $ Aeson.toJSON
                                [ Aeson.object
                                    [ "range" .= Aeson.object
                                      [ "start" .= Aeson.object
                                          [ "line" .= (0 :: Int)
                                          , "character" .= (0 :: Int)
                                          ]
                                      , "end" .= Aeson.object
                                          [ "line" .= max 0 (length srcLines - 1)
                                          , "character" .= case reverse srcLines of
                                                             a : _ -> BSC.length a
                                                             [] -> 0
                                          ]
                                      ]
                                    , "newText" .= stdout_
                                    ]
                                ]

    "textDocument/documentSymbol" ->
      do  let result =
                Aeson.parseEither (\obj ->
                  do  params <- obj .: "params"
                      requestID <- obj .: "id" :: Aeson.Parser Int

                      textDocument <- params .: "textDocument"
                      uri <- textDocument .: "uri" :: Aeson.Parser String

                      let filePath = drop 7 uri

                      return (requestID, filePath)
                ) =<< Aeson.eitherDecode body

          case result of
            Left err ->
              putStrFlushErr $ "Error decoding JSON: " ++ err

            Right (requestID, filePath) ->
              do  style <- Reporting.languageServer
                  symbolsResult <- getSymbols style state filePath

                  case symbolsResult of
                    Right symbols ->
                      respond requestID $ Aeson.toJSON $ map symbolInfoToJson symbols

                    Left err ->
                      respondErr requestID $ Reporting.Exit.toString $
                        definitionExitToReport filePath err

    unknownMethod ->
      putStrFlushErr $ "Unknown method: " ++ unknownMethod


putStrFlushErr :: String -> IO ()
putStrFlushErr str =
  IO.hPutStr IO.stderr str >> IO.hFlush IO.stderr


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
        text <- obj .: "text" :: Aeson.Parser String

        return (range, text)


getElmFormat :: Maybe String -> IO (Maybe FilePath)
getElmFormat maybeName =
  case maybeName of
    Just name ->
      Dir.findExecutable name

    Nothing ->
      Dir.findExecutable "elm-format"


applyChanges :: [((A.Position, A.Position), String)] -> BS.ByteString -> BS.ByteString
applyChanges changes content =
  List.foldl' (\acc ((start, end), newText) -> applyChange acc start end newText) content changes


applyChange :: BS.ByteString -> A.Position -> A.Position -> String -> BS.ByteString
applyChange content (A.Position sr sc) (A.Position er ec) newTextStr =
  let newText = BS_UTF8.fromString newTextStr
      lines_ = BSC.split '\n' content
      ( before, rest ) = splitAt (fromIntegral sr - 1) lines_
      ( startTargetLine, afterStart ) =
        case rest of
          a : b -> ( a, b )
          [] -> ( BS.empty, [] )

      endRest = drop (fromIntegral er - fromIntegral sr) rest

      ( endTargetLine, afterEnd ) =
        case endRest of
          a : b -> ( a, b )
          [] -> ( BS.empty, [] )

      ( start, _ ) = BSC.splitAt (fromIntegral sc - 1) startTargetLine
      ( _, end ) = BSC.splitAt (fromIntegral ec - 1) endTargetLine

      updated = BS.concat [ start, newText, end ]
  in BSC.intercalate (BS_UTF8.fromString "\n") $ before ++ (updated : afterEnd)


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


encodeRange :: A.Region -> Aeson.Value
encodeRange (A.Region (A.Position sr sc) (A.Position er ec)) =
  Aeson.object
    [ "start" .= Aeson.object
        [ "line" .= (sr - 1)
        , "character" .= (sc - 1)
        ]
    , "end" .= Aeson.object
        [ "line" .= (er - 1)
        , "character" .= (ec - 1)
        ]
    ]


encodeTextEdit :: A.Region -> Name -> Aeson.Value
encodeTextEdit region newName =
  Aeson.object
    [ "range" .= encodeRange region
    , "newText" .= (Name.toChars newName :: String)
    ]


encodeWorkspaceEdit :: Map.Map FilePath [A.Region] -> Name -> Aeson.Value
encodeWorkspaceEdit changes newName =
  Aeson.object
    [ "changes" .= Aeson.object
        (Map.foldrWithKey
          (\filePath regions acc ->
            (
              Aeson.Key.fromString ("file://" ++ filePath)
            ,
              Aeson.toJSON (map (`encodeTextEdit` newName) regions)
            ) : acc
          )
          []
          changes
        )
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


publishReportDiagnostic :: FilePath -> Int -> [Reporting.Report.Report] -> IO ()
publishReportDiagnostic filePath severity reports =
  sendNotification "textDocument/publishDiagnostics"
    (Aeson.object
      [ "uri" Aeson..= ("file://" ++ filePath :: String)
      , "diagnostics" Aeson..= map
        (\(Reporting.Report.Report title (A.Region (A.Position sr sc) (A.Position er ec)) _sgstns message) ->
          Aeson.object
            [ "range" Aeson..= Aeson.object
              [ "start" Aeson..= Aeson.object
                [ "line" Aeson..= (sr - 1)
                , "character" Aeson..= (sc - 1)
                ]
              , "end" Aeson..= Aeson.object
                [ "line" Aeson..= (er - 1)
                , "character" Aeson..= (ec - 1)
                ]
              ]
            , "severity" Aeson..= (severity :: Int)
            , "message" Aeson..= (map Char.toUpper title ++ "\n\n" ++ Reporting.Doc.toString message :: String)
            ]
        )
        reports
      ]
    )

-- DEFINITION


data DefinitionExit
  = DefinitionExitBadDetails Reporting.Exit.Details
  | DefinitionExitBadInput BS.ByteString Reporting.Error.Error
  | DefinitionExitNoRoot
  | DefinitionExitNotFound Element_
  | DefinitionExitNoElement
  | DefinitionExitModuleNotFound FilePath ModuleName.Raw
  | DefinitionExitNoFile FilePath
  | DefinitionExitNoProperModName Element_
  | DefinitionExitBadDownload Pkg.Name Version.Version Reporting.Exit.PackageProblem


definitionExitToReport :: FilePath -> DefinitionExit -> Reporting.Exit.Help.Report
definitionExitToReport path exit =
  case exit of
    DefinitionExitBadDetails details ->
      Reporting.Exit.toDetailsReport details

    DefinitionExitBadInput source error_ ->
      Reporting.Exit.Help.compilerReport "/" (Reporting.Error.Module "???" path File.zeroTime source error_) []

    DefinitionExitNoRoot ->
      Reporting.Exit.Help.report "DEFINITION FOR WHAT?" Nothing
        "I cannot find an elm.json so I am not sure where you want me to find things from."
        [ Reporting.Doc.reflow $
            "Elm packages always have an elm.json that says current the version number. If\
            \ you run this command from a directory with an elm.json file, I will try to bump\
            \ the version in there based on the API changes."
        ]

    DefinitionExitNotFound element ->
      -- FIXME: Add info about where looked for the definition in?
      Reporting.Exit.Help.report "NO DEFINITION" Nothing
        ("I tried to find the definition for " ++ elementToStr element ++ ", but failed to find it.")
        []

    DefinitionExitNoElement ->
      Reporting.Exit.Help.report "NO ELEMENT UNDER CURSOR" Nothing
        "I tried to find an element under the cursor, but could not."
        []

    DefinitionExitModuleNotFound root moduleName ->
      Reporting.Exit.Help.report "NO FILE FOR MODULE" Nothing
        ("I tried to find the file for " ++ ModuleName.toChars moduleName ++ ", but failed to find it in " ++ root ++ ".")
        []

    DefinitionExitNoFile path1 ->
      Reporting.Exit.Help.report "NO FILE" Nothing
        ("I tried to load a file from " ++ path1 ++ ", but it does not exist.")
        [ Reporting.Doc.reflow $
            "This often happens when there's nothing in the `~/.elm` folder.\
            \ You can try deleting the `elm-stuff` folder and running `elm make`\
            \ again to get the files to show up there."
        ]

    DefinitionExitNoProperModName element ->
      Reporting.Exit.Help.report "NO PROPER MODULE NAME" Nothing
        ("I tried to find the definition for " ++ elementToStr element ++ ", but failed to find it.")
        []

    DefinitionExitBadDownload pkg vsn packageProblem ->
      Reporting.Exit.toPackageProblemReport pkg vsn packageProblem


type Found = A.Located Found_


data Found_
  = FoundValue Src.Value
  | FoundPName Src.Expr Name
  | FoundDef Src.Expr Name
  | FoundInfix Src.Infix
  | FoundAlias Src.Alias
  | FoundUnion Src.Union
  | FoundVariant (A.Located Name, [Src.Type])
  | FoundModuleName Name


findDefinition ::
  Reporting.Style
  -> State
  -> FilePath
  -> A.Position
  -> IO (Either DefinitionExit (FilePath, Src.Module, Element, Found))
findDefinition style state filePath position =
  Task.run $
    do  root <- Task.mio DefinitionExitNoRoot $
           Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
        details <- Task.eio DefinitionExitBadDetails $
          BW.withScope $ \scope ->
            Stuff.withRootLock root (Details.load style scope root)

        findDefinitionHelp details root state filePath position


findDefinitionHelp ::
  Details.Details
  -> FilePath
  -> State
  -> FilePath
  -> A.Position
  -> Task.Task DefinitionExit (FilePath, Src.Module, Element, Found)
findDefinitionHelp details root state filePath position =
  Task.eio id $
  LsReporting.trackDefinition $
  Task.run $
  do  src <-
        case Details._outline details of
          Details.ValidApp _ -> loadSrcModuleByPath state filePath
          Details.ValidPkg name _ _ -> loadPkgModuleByPath state name filePath

      case findElement position src of
        Just element ->
          Task.eio id
            (findDefinitionForElement state details root filePath src (A.toValue element)
               & fmap (\a -> a & fmap (\(a1, a2, a3) -> (a1, a2, element, a3)))
            )

        Nothing ->
          Task.throw DefinitionExitNoElement


findDefinitionForElement ::
  State
  -> Details.Details
  -> FilePath
  -> FilePath
  -> Src.Module
  -> Element_
  -> IO (Either DefinitionExit (FilePath, Src.Module, Found))
findDefinitionForElement state details root path src element =
    case element of
      EVar defs patterns name ->
        -- FIXME: first found exposed low var is returned. Which may or may not be
        -- correct.
        --
        --
        -- Options:
        --  * return multiple options
        --  * do what the compiler does - return error
        --  * ignore; fixing the error in code, solves this problem.
        --
        --
        -- Example (elm-spa-example): Html.Attributes and Html both include a fn
        -- named `form`. About this, the compiler says:
        --
        --
        -- -- AMBIGUOUS NAME ------------------------------------------- src/Page/Login.elm
        --
        --   This usage of `form` is ambiguous:
        --
        --   122|     form [ onSubmit SubmittedForm ]
        --            ^^^^
        --   This name is exposed by 2 of your imports, so I am not sure which one to use:
        --
        --       Html.form
        --       Html.Attributes.form
        --
        --
        do  let local = findDefinitionForLowVarLocally src defs patterns name
            external <- findDefinitionForLowVarInImports state details root (Src._imports src) name

            return $
              case (local, external) of
                (Just a, _) -> Right (path, src, a)
                (Nothing, Right (Just a)) -> Right $ (\(a1, b1, c1) -> (a1, b1, fmap FoundValue c1)) a
                (Nothing, Left a) -> Left a
                (Nothing, Right Nothing) -> Left (DefinitionExitNotFound element)

      EVarQual prefix name ->
       fmap
         (\a -> a
           >>= fmap (\(a1, b1, c1) -> (a1, b1, fmap FoundValue c1)) . maybe (Left (DefinitionExitNotFound element)) Right
         )
         (findDefinitionForLowVarQualInImports state details root (Src._imports src) prefix name)

      ECtor name ->
        do  let local = findDefinitionForCtorInModule name src
            let potentialSources =
                  filter
                    (\import_@(Src.Import iName iAlias iExposing) ->
                      case iExposing of
                        Src.Open -> True
                        Src.Explicit exposedList -> any (\exposed ->
                            case exposed of
                              Src.Lower _ -> False
                              Src.Upper (A.At _ name_) Src.Private -> name_ == name
                              Src.Upper _ _ -> True
                              Src.Operator _ _ -> False
                          )
                          exposedList
                    )
                    (Src._imports src)
            external <- findDefinitionInImports state details root potentialSources $
                          findDefinitionForCtorInModule name

            return $
              case (local, external) of
                (Just a, _) -> Right (path, src, a)
                (Nothing, Right (Just a)) -> Right a
                (Nothing, Left a) -> Left a
                (Nothing, Right Nothing) -> Left (DefinitionExitNotFound element)

      ECtorQual qual name ->
        let
          potentialSources =
            filter
              (\(Src.Import iName iAlias _) ->
                if Maybe.isNothing iAlias then
                  qual == A.toValue iName
                else
                  Just qual == fmap A.toValue iAlias
              )
              (Src._imports src)
         in
         (findDefinitionInImports state details root potentialSources $
            findDefinitionForCtorInModule name
         )
            & fmap (\a -> a >>= maybe (Left (DefinitionExitNotFound element)) Right)

      EType name ->
        do  let local = findDefinitionForTypeInModule name src
            let potentialSources =
                  filter
                    (\import_@(Src.Import iName iAlias iExposing) ->
                      case iExposing of
                        Src.Open -> True
                        Src.Explicit exposedList -> any (\exposed ->
                            case exposed of
                              Src.Lower _ -> False
                              Src.Upper (A.At _ name_) _ -> name_ == name
                              Src.Operator _ _ -> False
                          )
                          exposedList
                    )
                    (Src._imports src)
            external <- findDefinitionInImports state details root potentialSources $
                          findDefinitionForTypeInModule name

            return $
              case (local, external) of
                (Just a, _) -> Right (path, src, a)
                (Nothing, Right (Just a)) -> Right a
                (Nothing, Left a) -> Left a
                (Nothing, Right Nothing) -> Left (DefinitionExitNotFound element)

      ETypeQual qual name ->
        let
          potentialSources =
            filter
              (\(Src.Import iName iAlias _) ->
                if Maybe.isNothing iAlias then
                  qual == A.toValue iName
                else
                  Just qual == fmap A.toValue iAlias
              )
              (Src._imports src)
         in
         (findDefinitionInImports state details root potentialSources $
            findDefinitionForTypeInModule name
         )
            & fmap (\a -> a >>= maybe (Left (DefinitionExitNotFound element)) Right)

      EAccess _ _ _ _ ->
        return (Left (DefinitionExitNoElement))

      EInfix name_ ->
        fmap
          (\a -> a
            >>= fmap (\(a1, b1, c1) -> (a1, b1, fmap FoundInfix c1)) . maybe (Left (DefinitionExitNotFound element)) Right
          )
          (findDefinitionForInfixInImports state details root (Src._imports src) name_)

      EModuleName name_ ->
        fmap
          (\a -> a
            >>= fmap (\(a1, b1, c1) -> (a1, b1, fmap FoundModuleName c1)) . maybe (Left (DefinitionExitNotFound element)) Right
          )
          (findDefinitionForModuleName state details root name_)


findDefinitionForModuleName ::
  State
  -> Details.Details
  -> FilePath
  -> ModuleName.Raw
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Name)))
findDefinitionForModuleName state details root moduleName =
  do  pathAndSrc <- loadSrcModule state details root moduleName

      case pathAndSrc of
        Right (path, src) ->
          return $ Right $ fmap (\a -> (path, src, a)) (Src._name src)

        Left exit ->
          return $ Left exit


findDefinitionForLowVarLocally ::
  Src.Module
  -> [(Src.Expr,A.Located Src.Def)]
  -> [(Src.Expr,Src.Pattern)]
  -> Name
  -> Maybe Found
findDefinitionForLowVarLocally (Src.Module _ _ _ _ values _ _ _ _) defs patterns name =
  let
    inDefs =
      foldr
        (\(expr,def) acc ->
          case A.toValue def of
            (Src.Define (A.At region valueName) _ _ _) ->
              if valueName == name then
                Just (A.At (A.toRegion def) (FoundDef expr name))
              else
                acc

            (Src.Destruct pattern _) ->
              fmap (\a -> A.At a (FoundPName expr name)) (findNameInPattern name pattern)
        )
        Nothing
        defs

    inPatterns =
      foldr
        (\(expr,p) acc ->
          fmap (\a -> A.At a (FoundPName expr name))
            (findNameInPattern name p) <|> acc
        )
        Nothing
        patterns

    inValues =
      foldr
        (\(A.At region value@(Src.Value (A.At _ valueName) _ _ _)) acc ->
          if valueName == name then Just (A.At region (FoundValue value)) else acc
        )
        Nothing
        values
  in
  inDefs <|> inPatterns <|> inValues


findDefinitionForLowVarQualInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> Name
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Src.Value)))
findDefinitionForLowVarQualInImports state details root imports qual name =
  let
    potentialSources =
      List.filter
        (\(Src.Import iName iAlias _) ->
          if Maybe.isNothing iAlias then
            qual == A.toValue iName
          else
            Just qual == fmap A.toValue iAlias
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, _) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    potentialSources


findDefinitionForLowVarInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Src.Value)))
findDefinitionForLowVarInImports state details root imports name =
  let
    potentialSources =
      filter
        (\import_@(Src.Import iName iAlias iExposing) ->
          case iExposing of
            Src.Open -> True
            Src.Explicit exposedList -> any (\exposed ->
                case exposed of
                  Src.Lower (A.At _ name_) -> name_ == name
                  Src.Upper _ _ -> False
                  Src.Operator _ _ -> False
              )
              exposedList
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, _) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    potentialSources


findDefinitionInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> (Src.Module -> Maybe Found)
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, Found)))
findDefinitionInImports state details root imports find =
  foldr
    (\import_ acc ->
      do  x <- findDefinitionInModule state details root (Src.getImportName import_) find
          y <- acc

          case (y, x) of
            (Left _, _) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    imports


findDefinitionInModule ::
  State
  -> Details.Details
  -> FilePath
  -> ModuleName.Raw
  -> (Src.Module -> Maybe Found)
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, Found)))
findDefinitionInModule state details root moduleName find =
  do  pathAndSrc <- loadSrcModule state details root moduleName

      case pathAndSrc of
        Right (path, src) ->
          return $ Right $ fmap (\a -> (path, src, a)) $ find src

        Left exit ->
          return $ Left exit


findDefinitionForCtorInModule :: Name -> Src.Module -> Maybe Found
findDefinitionForCtorInModule name src =
  let
    inAliases =
      foldr
        (\(A.At _ alias@(Src.Alias (A.At region aliasName) _ _)) acc ->
          if aliasName == name then Just (A.At region (FoundAlias alias)) else acc
        )
        Nothing
        (Src._aliases src)

    inUnions =
      foldr
        (\(A.At _ union@(Src.Union (A.At region unionName) _ variants)) acc ->
          foldr
            (\a acc1 ->
              if A.toValue (fst a) == name
                then Just (A.At (A.toRegion (fst a)) (FoundVariant a))
                else acc1
            )
            acc
            variants
        )
        Nothing
        (Src._unions src)
  in
  inAliases <|> inUnions


findDefinitionForTypeInModule :: Name -> Src.Module -> Maybe Found
findDefinitionForTypeInModule name src =
  let
    inAliases =
      foldr
        (\(A.At _ alias@(Src.Alias (A.At region aliasName) _ _)) acc ->
          if aliasName == name then Just (A.At region (FoundAlias alias)) else acc
        )
        Nothing
        (Src._aliases src)

    inUnions =
      foldr
        (\(A.At _ union@(Src.Union (A.At region unionName) _ variants)) acc ->
          if unionName == name
            then Just (A.At region (FoundUnion union))
            else acc
        )
        Nothing
        (Src._unions src)
  in
  inAliases <|> inUnions


findDefinitionForInfixInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Src.Infix)))
findDefinitionForInfixInImports state details root imports name =
  let
    potentialSources =
      filter
        (\import_@(Src.Import iName iAlias iExposing) ->
          case iExposing of
            Src.Open -> True
            Src.Explicit exposedList -> any (\exposed ->
                case exposed of
                  Src.Lower _ -> False
                  Src.Upper _ _ -> False
                  Src.Operator _ name_ -> name_ == name
              )
              exposedList
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForInfixInModule state details root (Src.getImportName import_) name

          case x of
            Right (Just _) -> return x
            _ -> acc
    )
    (return (Right Nothing))
    potentialSources


findDefinitionForInfixInModule ::
  State
  -> Details.Details
  -> FilePath
  -> ModuleName.Raw
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Src.Infix)))
findDefinitionForInfixInModule state details root moduleName name =
  do  pathAndSrc <- loadSrcModule state details root moduleName

      case pathAndSrc of
        Right (path, src) ->
          let def =
                foldr
                  (\infix_@(A.At _ (Src.Infix name_ _ _ _)) acc ->
                    if name_ == name then Just infix_ else acc
                  )
                  Nothing
                  (Src._binops src)

          in
          return $ Right (fmap (\a -> (path, src, a)) def)

        Left exit ->
          return $ Left exit


findDefinitionForNameInModule ::
  State
  -> Details.Details
  -> FilePath
  -> ModuleName.Raw
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, A.Located Src.Value)))
findDefinitionForNameInModule state details root moduleName name =
  do  pathAndSrc <- loadSrcModule state details root moduleName

      case pathAndSrc of
        Right (path, src) ->
          return $ Right $ fmap (\a -> (path, src, a)) $ findLowVarDefinitionNamed name src

        Left exit ->
          return $ Left exit

findLowVarDefinitionNamed :: Name -> Src.Module -> Maybe (A.Located Src.Value)
findLowVarDefinitionNamed name (Src.Module _ _ _ _ values _ _ _ _) =
  foldr (\value@(A.At _ (Src.Value name_ _ _ _)) acc ->
      if A.toValue name_ == name then Just value else acc
    )
    Nothing
    values


findNameInPattern :: Name -> Src.Pattern -> Maybe A.Region
findNameInPattern name pattern =
  case A.toValue pattern of
    Src.PAnything -> Nothing
    Src.PVar n    -> if n == name then Just (A.toRegion pattern) else Nothing

    Src.PRecord ns ->
      foldr (\(A.At r n) acc -> if n == name then Just r else acc) Nothing ns

    Src.PAlias p (A.At r n) ->
      if n == name then Just r else findNameInPattern name p

    Src.PUnit -> Nothing

    Src.PTuple a b cs ->
      foldr (\p acc -> findNameInPattern name p <|> acc) Nothing (a : b : cs)

    Src.PCtor _ _ ps ->
      foldr (\p acc -> findNameInPattern name p <|> acc) Nothing ps

    Src.PCtorQual _ _ _ _ -> Nothing

    Src.PList ps ->
      foldr (\p acc -> findNameInPattern name p <|> acc) Nothing ps

    Src.PCons hd tl ->
      findNameInPattern name hd <|> findNameInPattern name tl

    Src.PChr _ -> Nothing
    Src.PStr _ -> Nothing
    Src.PInt _ -> Nothing


type Element = A.Located Element_


data Element_
  = EVar [(Src.Expr,A.Located Src.Def)] [(Src.Expr,Src.Pattern)] Name
  | EVarQual Name Name
  | ECtor Name
  | ECtorQual Name Name
  | EType Name
  | ETypeQual Name Name
  | EAccess [(Src.Expr,A.Located Src.Def)] [(Src.Expr,Src.Pattern)] Src.Expr Name
  | EInfix Name
  | EModuleName Name


elementToStr :: Element_ -> String
elementToStr element =
  case element of
    EVar _ _ name ->
      Name.toChars name ++ " (Var)"
    EVarQual prefix name ->
      Name.toChars prefix ++ "." ++ Name.toChars name ++ " (VarQual)"
    ECtor name ->
      Name.toChars name ++ " (Ctor)"
    ECtorQual prefix name ->
      Name.toChars prefix ++ "." ++ Name.toChars name ++ " (CtorQual)"
    EType name ->
      Name.toChars name ++ " (Type)"
    ETypeQual prefix name ->
      Name.toChars prefix ++ "." ++ Name.toChars name ++ " (TypeQual)"
    EAccess _ _ record field ->
      "." ++ Name.toChars field ++ " (Access)"
    EInfix name ->
      Name.toChars name ++ " (Infix)"
    EModuleName name ->
      Name.toChars name ++ " (Module)"


elementToRenamePlaceholder :: Element -> String
elementToRenamePlaceholder element =
  case A.toValue element of
    EVar _ _ name ->
      Name.toChars name
    EVarQual prefix name ->
      Name.toChars name
    ECtor name ->
      Name.toChars name
    ECtorQual prefix name ->
      Name.toChars name
    EType name ->
      Name.toChars name
    ETypeQual prefix name ->
      Name.toChars name
    EAccess _ _ record field ->
      Name.toChars field
    EInfix name ->
      Name.toChars name
    EModuleName name ->
      Name.toChars name


findElement :: A.Position -> Src.Module -> Maybe Element
findElement pos src =
  maybe Nothing
    (\a ->
      if isInRegion pos (A.toRegion a) then
        Just (A.At (A.toRegion a) (EModuleName (A.toValue a)))
      else
        Nothing
    )
    (Src._name src) <|>
  findElementInExports pos (Src._exports src) <|>
  findElementInValues pos (Src._values src) <|>
  findElementInAliases pos (Src._aliases src) <|>
  findElementInUnions pos (Src._unions src) <|>
  findElementInImports pos (Src._imports src)


findElementInExports :: A.Position -> A.Located Src.Exposing -> Maybe Element
findElementInExports pos exposing =
  if isInRegion pos (A.toRegion exposing) then
    case A.toValue exposing of
      Src.Open -> Nothing
      Src.Explicit exposed ->
        foldr
          (\a acc ->
            case a of
              Src.Lower name ->
                if isInRegion pos (A.toRegion name)
                  then Just $ A.At (A.toRegion name) $ EVar [] [] (A.toValue name)
                  else acc
              Src.Upper name _ ->
                if isInRegion pos (A.toRegion name)
                  then Just $ A.At (A.toRegion name) $ EType (A.toValue name)
                  else acc
              Src.Operator region name ->
                if isInRegion pos region
                  then Just $ A.At region $ EInfix name
                  else acc
          )
          Nothing
          exposed
  else
    Nothing


findElementInAliases :: A.Position -> [A.Located Src.Alias] -> Maybe Element
findElementInAliases pos aliases =
  foldr
    (\(A.At region (Src.Alias name _ type_)) found ->
      let inName =
            if isInRegion pos (A.toRegion name)
              then Just (A.At (A.toRegion name) (EType (A.toValue name)))
              else Nothing

          inType = findElementInType pos type_
      in
      inName <|> inType <|> found
    )
    Nothing
    aliases


findElementInUnions :: A.Position -> [A.Located Src.Union] -> Maybe Element
findElementInUnions pos unions =
  foldr
    (\(A.At region (Src.Union name _ variants)) found ->
      let inName =
            if isInRegion pos (A.toRegion name)
              then Just (A.At (A.toRegion name) (EType (A.toValue name)))
              else Nothing

          inVariants =
            foldr
              (\(name_, types) found_ ->
                let a_ =
                      if isInRegion pos (A.toRegion name_)
                        then Just (A.At (A.toRegion name_) (ECtor (A.toValue name_)))
                        else Nothing

                    b_ =
                      foldr (\a acc -> findElementInType pos a <|> acc) Nothing types
                in
                a_ <|> b_ <|> found_

              )
              Nothing
              variants
      in
      inName <|> inVariants <|> found
    )
    Nothing
    unions


findElementInImports :: A.Position -> [Src.Import] -> Maybe Element
findElementInImports pos imports =
  foldr
    (\(Src.Import iName alias exposing) found ->
      let inNameOrAlias =
            if isInRegion pos (A.toRegion iName)
              then Just (A.At (A.toRegion iName) (EModuleName (A.toValue iName)))
              else
                maybe
                  Nothing
                    (\a ->
                      if isInRegion pos (A.toRegion a)
                        then Just (A.At (A.toRegion iName) (EModuleName (A.toValue iName)))
                        else Nothing
                    )
                    alias

          inExposing =
            case exposing of
              Src.Open ->
                Nothing

              Src.Explicit exposed ->
                foldr
                  (\a acc ->
                    case a of
                      Src.Lower name ->
                        if isInRegion pos (A.toRegion name)
                          then Just (A.At (A.toRegion name) (EVar [] [] (A.toValue name)))
                          else acc

                      Src.Upper name _ ->
                        if isInRegion pos (A.toRegion name)
                          then Just (A.At (A.toRegion name) (EType (A.toValue name)))
                          else acc

                      Src.Operator region name ->
                        if isInRegion pos region
                          then Just (A.At region (EInfix name))
                          else acc
                  )
                  Nothing
                  exposed
      in
      inNameOrAlias <|> inExposing <|> found
    )
    Nothing
    imports


findElementInValues :: A.Position -> [A.Located Src.Value] -> Maybe Element
findElementInValues pos values =
  foldr
    (\located found ->
      case located of
        A.At valueRegion (Src.Value name patterns body type_) ->
          let inPatterns =
                  List.foldr
                    (\pattern@(A.At region _) acc ->
                      if isInRegion pos region then
                        findElementInPattern pos [] (List.map (\a -> (body,a)) patterns) pattern
                      else
                        acc
                    )
                    Nothing
                    patterns

              inValueNameOrBody =
                if isPositionOnValueName pos located
                  then Just (A.At (A.toRegion name) (EVar [] [] (A.toValue name)))
                else if isInRegion pos valueRegion
                  then findElementInExpr pos [] (List.map (\a -> (body,a)) patterns) body
                else Nothing

              inType = findElementInType pos Control.Monad.=<< type_
          in
          inPatterns <|> inValueNameOrBody <|> inType <|> found
    )
    Nothing
    values


isPositionOnValueName :: A.Position -> A.Located Src.Value -> Bool
isPositionOnValueName pos value =
    let (A.At (A.Region (A.Position sx sy) _) (Src.Value name _ _ typeAnn)) = value
        valNameLen = fromIntegral (length (Name.toChars (A.toValue name)))
    in
    isInRegion pos (A.toRegion name)
      || (Maybe.isJust typeAnn
           && isInRegion pos (A.Region (A.Position sx 0) (A.Position sx valNameLen))
         )


findElementInType :: A.Position -> Src.Type -> Maybe Element
findElementInType pos type_ =
  if isInRegion pos (A.toRegion type_)
    then
      case A.toValue type_ of
        Src.TLambda arg ret ->
          findElementInType pos arg <|> findElementInType pos ret

        Src.TVar name ->
          Nothing

        Src.TType region name tlist ->
          if isInRegion pos region
            then Just (A.At region (EType name))
            else foldr (\a acc -> findElementInType pos a <|> acc) Nothing tlist

        Src.TTypeQual region qual name tlist ->
          if isInRegion pos region
            then Just (A.At region (ETypeQual qual name))
            else foldr (\a acc -> findElementInType pos a <|> acc) Nothing tlist

        Src.TRecord fields extRecord ->
            foldr (\a acc -> findElementInType pos (snd a) <|> acc) Nothing fields

        Src.TUnit ->
          Nothing

        Src.TTuple a b cs ->
            foldr (\x acc -> findElementInType pos x <|> acc) Nothing (a : b : cs)

    else
      Nothing


isInRegion :: A.Position -> A.Region -> Bool
isInRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
    && (row == endRow && col <= endCol || row < endRow)


findElementInExpr
  :: A.Position
  -> [(Src.Expr,A.Located Src.Def)]
  -> [(Src.Expr,Src.Pattern)]
  -> Src.Expr
  -> Maybe Element
findElementInExpr pos defs patterns expr =
  case A.toValue expr of
    Src.Chr   _ -> Nothing
    Src.Str   _ -> Nothing
    Src.Int   _ -> Nothing
    Src.Float _ -> Nothing

    Src.Var varType name ->
      case varType of
        Src.LowVar -> Just $ A.At (A.toRegion expr) $ EVar defs patterns name
        Src.CapVar -> Just $ A.At (A.toRegion expr) $ ECtor name

    Src.VarQual varType prefix name ->
      case varType of
        Src.LowVar -> Just $ A.At (A.toRegion expr) $ EVarQual prefix name
        Src.CapVar -> Just $ A.At (A.toRegion expr) $ ECtorQual prefix name

    Src.List exprs ->
      foldr
        (\a acc ->
          if isInRegion pos (A.toRegion a) then
            findElementInExpr pos defs patterns a
          else
            acc
        )
        Nothing
        exprs

    Src.Op name ->
      Just $ A.At (A.toRegion expr) $ EInfix name

    Src.Negate e ->
      if isInRegion pos (A.toRegion e) then
        findElementInExpr pos defs patterns e
      else
        Nothing

    Src.Binops ops final ->
      if isInRegion pos (A.toRegion final) then
        findElementInExpr pos defs patterns final
      else
        foldr
          (\(e, op) acc ->
            if isInRegion pos (A.toRegion op) then
              Just $ A.At (A.toRegion op) $ EInfix (A.toValue op)
            else if isInRegion pos (A.toRegion e) then
              findElementInExpr pos defs patterns e
            else
              acc
          )
          Nothing
          ops

    Src.Lambda srcArgs body ->
      let ps = List.map (\a -> (body,a)) srcArgs in
      if isInRegion pos (A.toRegion body) then
        findElementInExpr pos defs (ps ++ patterns) body
      else
        List.foldr
          (\arg acc ->
            if isInRegion pos (A.toRegion arg) then
              findElementInPattern pos defs (ps ++ patterns) arg
            else
              acc
          )
          Nothing
          srcArgs

    Src.Call func args ->
      if isInRegion pos (A.toRegion func) then
        findElementInExpr pos defs patterns func
      else
        foldr
          (\a acc ->
            if isInRegion pos (A.toRegion a) then
              findElementInExpr pos defs patterns a
            else
              acc
          )
          Nothing
          args

    Src.If branches finally_ ->
      if isInRegion pos (A.toRegion finally_) then
        findElementInExpr pos defs patterns finally_
      else
        foldr
          (\(condition, branch) acc ->
            if isInRegion pos (A.toRegion condition) then
              findElementInExpr pos defs patterns condition
            else if isInRegion pos (A.toRegion branch) then
              findElementInExpr pos defs patterns branch
            else
              acc
          )
          Nothing
          branches

    Src.Let defs1 body ->
      let letDefs = List.map (\a -> (expr,a)) defs1 ++ defs in
      if isInRegion pos (A.toRegion body) then
        findElementInExpr pos letDefs patterns body
      else
        foldr
          (\def acc ->
              case A.toValue def of
                Src.Define name ps e t ->
                  let
                      onName =
                        if isInRegion pos (A.toRegion name) then
                          Just $ A.At (A.toRegion name) $ EVar letDefs patterns (A.toValue name)
                        else
                          Nothing

                      onNameInType =
                        case t of
                          Just _ ->
                            let
                              (A.Region (A.Position r c) _) = A.toRegion def
                              nameLen = fromIntegral (length (Name.toChars (A.toValue name)))
                              region = A.Region (A.Position r c) (A.Position r (c + nameLen))
                            in
                            if isInRegion pos region then
                              Just $ A.At region $ EVar letDefs patterns (A.toValue name)
                            else
                              Nothing

                          Nothing ->
                            Nothing

                      newPatterns = List.map (\a -> (body, a)) ps ++ patterns

                      inPatterns =
                        List.foldr
                          (\p acc1 ->
                            if isInRegion pos (A.toRegion p) then
                              findElementInPattern pos letDefs newPatterns p
                            else
                              acc1
                          )
                          Nothing
                          ps

                      inExpr =
                        if isInRegion pos (A.toRegion e) then
                          findElementInExpr pos letDefs newPatterns e
                        else
                          acc

                      inType = findElementInType pos Control.Monad.=<< t
                  in
                  onName <|> inPatterns <|> inExpr <|> inType <|> acc

                Src.Destruct p e ->
                  if isInRegion pos (A.toRegion p) then
                    findElementInPattern pos letDefs ((expr,p) : patterns) p
                  else if isInRegion pos (A.toRegion e) then
                    findElementInExpr pos letDefs ((expr,p) : patterns) e
                  else
                    acc
          )
          Nothing
          defs1

    Src.Case e bs ->
      if isInRegion pos (A.toRegion e) then
        findElementInExpr pos defs patterns e
      else
        foldr
          (\(p, b) acc ->
            if isInRegion pos (A.toRegion p) then
              findElementInPattern pos defs ((expr,p) : patterns) p
            else if isInRegion pos (A.toRegion b) then
              findElementInExpr pos defs ((expr,p) : patterns) b
            else
              acc
          )
          Nothing
          bs

    Src.Accessor field ->
      Nothing

    Src.Access record field ->
      if isInRegion pos (A.toRegion field) then
        Just $ A.At (A.toRegion field) $ EAccess defs patterns record (A.toValue field)
      else if isInRegion pos (A.toRegion record) then
        findElementInExpr pos defs patterns record
      else
        Nothing

    Src.Update starter fields ->
      if isInRegion pos (A.toRegion starter) then
        Just $ A.At (A.toRegion starter) $ EVar defs patterns (A.toValue starter)

      else
        foldr
          (\(field, value) acc ->
            if isInRegion pos (A.toRegion field) then
              Just $ A.At (A.toRegion field) $ EVar defs patterns (A.toValue field)
            else if isInRegion pos (A.toRegion value) then
              findElementInExpr pos defs patterns value
            else
              acc
          )
          Nothing
          fields

    Src.Record fields ->
      foldr
        (\(field, value) acc ->
          if isInRegion pos (A.toRegion value) then
            findElementInExpr pos defs patterns value
          else
            acc
        )
        Nothing
        fields

    Src.Unit ->
      Nothing

    Src.Tuple a b cs ->
      foldr
        (\e es ->
          if isInRegion pos (A.toRegion e) then
            findElementInExpr pos defs patterns e
          else
            es
        )
        Nothing
        (a : b : cs)

    Src.Shader _ _ ->
      Nothing


findElementInPattern
  :: A.Position
  -> [(Src.Expr,A.Located Src.Def)]
  -> [(Src.Expr,Src.Pattern)]
  -> Src.Pattern
  -> Maybe Element
findElementInPattern pos rootDefs rootPatterns rootPattern =
  case A.toValue rootPattern of
    Src.PAnything ->
      Nothing

    Src.PVar name ->
      Just $ A.At (A.toRegion rootPattern) $
        EVar rootDefs rootPatterns name

    Src.PRecord names ->
      List.foldr
        (\(A.At region name) acc ->
          if isInRegion pos region then
            Just $ A.At region $ EVar rootDefs rootPatterns name
          else
            acc
        )
        Nothing
        names

    Src.PAlias pattern (A.At region alias) ->
      if isInRegion pos region then
        Just $ A.At region $ EVar rootDefs rootPatterns alias
      else
        findElementInPattern pos rootDefs rootPatterns pattern

    Src.PUnit ->
      Nothing

    Src.PTuple a b rest ->
      List.foldr
        (\pattern@(A.At region _) acc ->
          if isInRegion pos region then
            findElementInPattern pos rootDefs rootPatterns pattern
          else
            acc
        )
        Nothing
        (a : b : rest)

    Src.PCtor region name args ->
      let
        maybeElement =
          List.foldr
            (\pattern acc ->
              if isInRegion pos (A.toRegion pattern) then
                findElementInPattern pos rootDefs rootPatterns pattern
              else
                acc
            )
            Nothing
            args
      in
      maybeElement <|>
        if isInRegion pos region then
          Just $ A.At region $ ECtor name
        else
          Nothing


    Src.PCtorQual region qual name args ->
      let
        maybeElement =
          List.foldr
            (\pattern acc ->
              if isInRegion pos (A.toRegion pattern) then
                findElementInPattern pos rootDefs rootPatterns pattern
              else
                acc
            )
            Nothing
            args
      in
      maybeElement <|>
        if isInRegion pos region then
          Just $ A.At region $ ECtorQual qual name
        else
          Nothing

    Src.PList patternList ->
        List.foldr
          (\pattern acc ->
            if isInRegion pos (A.toRegion pattern) then
              findElementInPattern pos rootDefs rootPatterns pattern
            else
              acc
          )
          Nothing
          patternList

    Src.PCons a b ->
      findElementInPattern pos rootDefs rootPatterns a <|>
        findElementInPattern pos rootDefs rootPatterns b

    Src.PChr _ ->
      Nothing

    Src.PStr _ ->
      Nothing

    Src.PInt _ ->
      Nothing



-- REFERENCES


data RefsEnv =
  RefsEnv
    { _state :: State
    , _root :: FilePath
    , _details :: Details.Details
    }


findReferences ::
  Reporting.Style
  -> State
  -> FilePath
  -> A.Position
  -> IO (Either DefinitionExit (Map.Map FilePath [A.Region]))
findReferences style state filePath position =
  Task.run $
    do  root <- Task.mio DefinitionExitNoRoot $
           Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
        details <- Task.eio DefinitionExitBadDetails $
          BW.withScope $ \scope ->
            Stuff.withRootLock root (Details.load style scope root)

        localSrc <-
          case Details._outline details of
            Details.ValidApp _ -> loadSrcModuleByPath state filePath
            Details.ValidPkg name _ _ -> loadPkgModuleByPath state name filePath

        (modulePath, src, _, found) <- findDefinitionHelp details root state filePath position

        Task.io $ findReferencesHelp (RefsEnv state root details) modulePath src found


findReferencesHelp :: RefsEnv -> FilePath -> Src.Module -> Found -> IO (Map.Map FilePath [A.Region])
findReferencesHelp (RefsEnv state root details) modulePath defSrc found =
  LsReporting.trackReferences $ \key ->
  case A.toValue found of
    FoundValue value@(Src.Value (A.At _ name) _ _ _) ->
      do  let local =
                List.concatMap (findVarInValue name . A.toValue) (Src._values defSrc)
                ++ valueRegions (A.At (A.toRegion found) value)
                ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                      & maybe [] (\a -> [a])
                   )
          LsReporting.report key (RefsDone (length local))
          foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a

                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      do  let imported =
                                findReferencesForImportedVar (Src.getName defSrc) name src

                          LsReporting.report key (RefsDone (length imported))
                          fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

    FoundAlias value@(Src.Alias (A.At region name) _ _) ->
      do  let local =
                region : findNameInModuleTypes name defSrc
                ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                     & maybe [] (\a -> [a])
                   )
          LsReporting.report key (RefsDone (length local))
          foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      let
                        maybeImport =
                          List.find
                            (\import_ -> A.toValue (Src._import import_) == Src.getName defSrc)
                            (Src._imports src)

                        imported =
                          case maybeImport of
                            Just import_@(Src.Import _ alias exposing) ->
                              let
                                qual = maybe (Src.getImportName import_) A.toValue alias

                                qualInModule =
                                  findQualNameInModuleTypes qual name src
                                    & map (keepOnlyNameRegionInVarQualRegion name)
                              in
                              case findNameInExposing name exposing of
                                Just importRegion ->
                                  importRegion : findNameInModuleTypes name src ++ qualInModule

                                Nothing ->
                                  qualInModule

                            Nothing -> []
                      in
                      do  LsReporting.report key (RefsDone (length imported))
                          fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

    FoundUnion (Src.Union (A.At region name) _ _) ->
      do  let local =
                region : findNameInModuleTypes name defSrc
                ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                     & maybe [] (\a -> [a])
                   )
          LsReporting.report key (RefsDone (length local))
          foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      let
                        maybeImport =
                          List.find
                            (\import_ -> A.toValue (Src._import import_) == Src.getName defSrc)
                            (Src._imports src)

                        imported =
                          case maybeImport of
                            Just import_@(Src.Import _ alias exposing) ->
                              let
                                qual = maybe (Src.getImportName import_) A.toValue alias

                                qualInModule =
                                  findQualNameInModuleTypes qual name src
                                    & map (keepOnlyNameRegionInVarQualRegion name)
                              in
                              case findNameInExposing name exposing of
                                Just importRegion ->
                                  importRegion : findNameInModuleTypes name src ++ qualInModule

                                Nothing ->
                                  qualInModule

                            Nothing -> []
                      in
                      do  LsReporting.report key (RefsDone (length imported))
                          fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

    FoundVariant (A.At _ name, _) ->
      do  let local =
                (A.toRegion found)
                  : List.concatMap (findVarInValue name . A.toValue) (Src._values defSrc)
                  ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                        & maybe [] (\a -> [a])
                     )
          LsReporting.report key (RefsDone (length local))
          foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a

                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      do  let imported =
                                findReferencesForImportedVar (Src.getName defSrc) name src

                          LsReporting.report key (RefsDone (length imported))
                          fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

    FoundInfix (Src.Infix name _ _ _) ->
      do  let local = infixInModule name defSrc
          LsReporting.report key (RefsDone (length local))
          foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      let
                        maybeImport =
                            List.find
                              (\import_ -> A.toValue (Src._import import_) == Src.getName defSrc)
                              (Src._imports src)

                        imported =
                          case maybeImport of
                            Just import_@(Src.Import _ alias _) ->
                              if isInfixExposed import_ name then
                                infixInModule name src

                              else
                                []

                            Nothing -> []
                      in
                      do  LsReporting.report key (RefsDone (length imported))
                          fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

    FoundPName expr name ->
      let
        exprWithoutFoundDef =
          case A.toValue expr of
            Src.Let ds e ->
              A.At (A.toRegion expr)
                (Src.Let (List.filter (\a -> not (isFoundDef a)) ds) e)

            _ ->
              expr

        isFoundDef def =
          case A.toValue def of
            Src.Define _ _ _ _ -> False
            Src.Destruct p _   -> Maybe.isJust (findNameInPattern name p)
      in
      return $ Map.singleton modulePath (A.toRegion found : varInExpr name [] exprWithoutFoundDef)

    FoundDef expr name ->
      let
        exprWithoutFoundDef =
          case A.toValue expr of
            Src.Let ds e ->
              A.At (A.toRegion expr)
                (Src.Let (List.filter (\a -> not (isFoundDef a)) ds) e)

            _ ->
              expr

        isFoundDef def =
          case A.toValue def of
            Src.Define (A.At _ n) _ _ _ -> n == name
            Src.Destruct _ _            -> False
        in
        return $ Map.singleton modulePath $
          A.toRegion found : varInExpr name [] exprWithoutFoundDef

    FoundModuleName name ->
      do  LsReporting.report key (RefsDone 1)
          foldr
            (\importer acc ->
              do  loadResult <- loadSrcModule state details root importer

                  case loadResult of
                    Left _ -> acc -- ignore not found - deleted files included

                    Right (path, src) ->
                      let
                        maybeImport =
                          List.find (\a -> A.toValue (Src._import a) == name)
                            (Src._imports src)
                      in
                        case maybeImport of
                          Just (Src.Import n _ _) ->
                            do  LsReporting.report key (RefsDone 1)
                                fmap (Map.insert path [A.toRegion n]) acc

                          Nothing ->
                            acc
            )
            (return $ Map.singleton modulePath [A.toRegion found])
            (importersOf details name)


findReferencesForImportedVar :: ModuleName.Raw -> Name -> Src.Module -> [A.Region]
findReferencesForImportedVar moduleName name src =
  case List.find (\a -> A.toValue (Src._import a) == moduleName) (Src._imports src) of
    Just import_@(Src.Import _ alias exposing) ->
      let
        qual = maybe (Src.getImportName import_) A.toValue alias

        qualInValues =
          Src._values src
            & List.concatMap
                (\(A.At _ (Src.Value _ _ expr _)) ->
                  varQualInExpr qual name [] expr
                )
            & map (keepOnlyNameRegionInVarQualRegion name)
      in
      case findNameInExposing name exposing of
        Just importRegion ->
          let inValues = concatMap (findVarInValue name . A.toValue) (Src._values src) in
          importRegion : inValues ++ qualInValues

        Nothing ->
          qualInValues

    Nothing -> []


keepOnlyNameRegionInVarQualRegion :: Name -> A.Region -> A.Region
keepOnlyNameRegionInVarQualRegion name (A.Region _ (A.Position endRow endCol)) =
  let
    nameLength = fromIntegral (length (Name.toChars name))
  in
  A.Region (A.Position endRow (endCol - nameLength)) (A.Position endRow endCol)


loadSrcModule :: State -> Details.Details -> FilePath -> ModuleName.Raw -> IO (Either DefinitionExit (FilePath, Src.Module))
loadSrcModule state details root moduleName =
  do  files <- Control.Concurrent.MVar.readMVar (_changedFiles state)
      let localPath = fmap Details._path $ Map.lookup moduleName $ Details._locals details

      case localPath of
        Just local ->
          do  let projectType =
                    case Details._outline details of
                      Details.ValidApp _ -> Parse.Application
                      Details.ValidPkg pkgName _ _ -> Parse.Package pkgName

              fileExists <- File.exists local

              if not fileExists then
                return $ Left $ DefinitionExitNoFile local

              else
                do  source <-
                      maybe (File.readUtf8 local) return $
                        Map.lookup local files

                    return $
                      Data.Bifunctor.first (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                        (fmap ((,) local) (Parse.fromByteString projectType source))

        Nothing ->
          do  let maybePkg = lookupPkgName details moduleName
              maybeVsn <- case maybePkg of
                            Just pkg -> getPackageCurrentlyUsedOrLatestVersion "." pkg
                            Nothing -> return Nothing

              case (maybePkg, maybeVsn) of
                (Just pkg, Just vsn) ->
                  do  let projectType = Parse.Package pkg

                      cache <- Stuff.getPackageCache
                      let home = Stuff.package cache pkg vsn
                      let path = home </> "src" </> ModuleName.toFilePath moduleName <.> "elm"

                      pkgExists <- Dir.doesDirectoryExist (home </> "src")
                      fileExists <- File.exists path

                      if pkgExists && fileExists then
                        do  source <-
                              maybe (File.readUtf8 path) return $
                                Map.lookup path files

                            return $
                              Data.Bifunctor.first (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                                (fmap ((,) path) (Parse.fromByteString projectType source))

                      else
                        do  let workDoneToken = "package-download"
                            sendCreateWorkDoneProgress workDoneToken
                            sendProgressBegin workDoneToken ("⬇️ Downloading " ++ Pkg.toChars pkg ++ "/" ++ Version.toChars vsn)

                            startTime <- Data.Time.getCurrentTime

                            manager <- Http.getManager

                            Dir.createDirectoryIfMissing True (Stuff.package cache pkg vsn)
                            result <- Details.downloadPackage cache manager pkg vsn

                            endTime <- Data.Time.getCurrentTime
                            sendProgressEnd workDoneToken $
                               "Done in " ++ show (Data.Time.diffUTCTime endTime startTime)

                            case result of
                              Left problem ->
                                return $ Left $ DefinitionExitBadDownload pkg vsn problem

                              Right () ->
                                do  source <-
                                      maybe (File.readUtf8 path) return $
                                        Map.lookup path files

                                    return $
                                      Data.Bifunctor.first (DefinitionExitBadInput source . Reporting.Error.BadSyntax) $
                                        (fmap ((,) path) (Parse.fromByteString projectType source))

                _ ->
                  return $ Left $ DefinitionExitModuleNotFound root moduleName


lookupPkgName :: Details.Details -> ModuleName.Raw -> Maybe Pkg.Name
lookupPkgName details canModuleName =
  fmap (\(Details.Foreign name_ _) -> name_) $
    Map.lookup canModuleName $
    Details._foreigns details


loadSrcModuleByPath :: State -> FilePath -> Task.Task DefinitionExit Src.Module
loadSrcModuleByPath state filePath =
  do  files <- Task.io $ Control.Concurrent.MVar.readMVar (_changedFiles state)
      source <- maybe (Task.io $ File.readUtf8 filePath) return $ Map.lookup filePath files

      case Parse.fromByteString Parse.Application source of
        Left err ->
          Task.throw (DefinitionExitBadInput source (Reporting.Error.BadSyntax err))

        Right module_ ->
          return module_


loadPkgModuleByPath :: State -> Pkg.Name -> FilePath -> Task.Task DefinitionExit Src.Module
loadPkgModuleByPath state pkgName filePath =
  do  files <- Task.io $ Control.Concurrent.MVar.readMVar (_changedFiles state)
      source <- maybe (Task.io $ File.readUtf8 filePath) return $ Map.lookup filePath files

      case Parse.fromByteString (Parse.Package pkgName) source of
        Left err ->
          Task.throw (DefinitionExitBadInput source (Reporting.Error.BadSyntax err))

        Right module_ ->
          return module_


isLowVarExposed :: Src.Import -> Name -> Bool
isLowVarExposed (Src.Import _ _ Src.Open) name = True
isLowVarExposed (Src.Import _ _ (Src.Explicit exposing)) name =
  any
    (\exposing_ ->
      case exposing_ of
        Src.Lower (A.At _ name_) -> name == name_
        Src.Upper _ _ -> False
        Src.Operator _ _ -> False
    )
    exposing


isInfixExposed :: Src.Import -> Name -> Bool
isInfixExposed (Src.Import _ _ Src.Open) name = True
isInfixExposed (Src.Import _ _ (Src.Explicit exposing)) name =
  any
    (\exposing_ ->
      case exposing_ of
        Src.Lower (A.At _ name_) -> False
        Src.Upper _ _ -> False
        Src.Operator _ name_ -> name == name_
    )
    exposing


importersOf :: Details.Details -> ModuleName.Raw -> Set.Set ModuleName.Raw
importersOf details targetModule =
  let locals = Details._locals details in
  Map.foldrWithKey
    (\localModuleName localDetails found ->
      if List.elem targetModule (Details._deps localDetails)
        then Set.insert localModuleName found
        else found
    )
    Set.empty
    locals


findVarInValue :: Name -> Src.Value -> [A.Region]
findVarInValue name (Src.Value _ patterns expr _) =
  -- Find out what this is about; the commit that added it says
  -- "find modules correctly from inside packages"
  if any (Maybe.isJust . findNameInPattern name) patterns
    then []
    else varInExpr name [] expr


valueRegions :: A.Located Src.Value -> [A.Region]
valueRegions (A.At region (Src.Value name _ _ type_)) =
  case type_ of
    Just _ ->
      [ let
          (A.Region (A.Position startRow _) _) = region
          nameLength = fromIntegral (length (Name.toChars (A.toValue name)))
        in
        A.Region (A.Position startRow 1) (A.Position startRow (nameLength + 1))
      , A.toRegion name
      ]

    Nothing ->
      [ A.toRegion name ]


findNameInExposing :: Name -> Src.Exposing -> Maybe A.Region
findNameInExposing name exposing =
  case exposing of
    Src.Open -> Nothing
    Src.Explicit exposed ->
      foldr (\a found -> found <|> findNameInExposed name a) Nothing exposed


findNameInExposed :: Name -> Src.Exposed ->  Maybe A.Region
findNameInExposed name exposed =
  case exposed of
    Src.Lower (A.At region name_) -> if name == name_ then Just region else Nothing
    Src.Upper (A.At region name_) _ -> if name == name_ then Just region else Nothing
    Src.Operator _ _ -> Nothing


varInExpr :: Name -> [A.Region] -> Src.Expr -> [A.Region]
varInExpr name regions rootExpr =
  case A.toValue rootExpr of
    Src.Chr _   -> regions
    Src.Str _   -> regions
    Src.Int _   -> regions
    Src.Float _ -> regions

    Src.Var _ varName ->
      if varName == name then A.toRegion rootExpr : regions else regions

    Src.VarQual _ _ _ -> regions
    Src.List exprs    -> List.foldl (varInExpr name) regions exprs
    Src.Op _          -> regions
    Src.Negate expr   -> varInExpr name regions expr

    Src.Binops exprsAndNames expr ->
      List.foldl (\a (expr_, _) -> varInExpr name a expr_)
        (varInExpr name regions expr)
        exprsAndNames

    Src.Lambda patterns expr ->
      if any (Maybe.isJust . findNameInPattern name) patterns
        then regions
        else varInExpr name regions expr

    Src.Call func args -> List.foldl (varInExpr name) (varInExpr name regions func) args

    Src.If listTupleExprs expr ->
      List.foldl
        (\a (one, two) ->
          varInExpr name (varInExpr name a one) two
        )
        (varInExpr name regions expr)
        listTupleExprs

    Src.Let defs expr ->
      let
        isNameInDefs =
          any
            (\a ->
              case A.toValue a of
                Src.Define (A.At _ name_) _ _ _ -> name == name_
                Src.Destruct pattern _ ->
                  Maybe.isJust (findNameInPattern name pattern)
            )
          defs
      in
      if isNameInDefs then
        regions
      else
        List.foldl
          (\a (A.At _ def_) ->
            case def_ of
              Src.Define (A.At _ name_) _ expr_ _ -> varInExpr name a expr_
              Src.Destruct pattern expr_ -> varInExpr name a expr_
          )
          (varInExpr name regions expr)
          defs

    Src.Case expr branches ->
      if any (Maybe.isJust . findNameInPattern name . fst) branches
        then regions
        else List.foldl
          (\a (_, x) ->
            varInExpr name (varInExpr name a x) expr
          )
          (varInExpr name regions expr)
          branches

    Src.Accessor _      -> regions
    Src.Access expr _   -> varInExpr name regions expr
    Src.Update _ fields -> List.foldl (\a (_, x) -> varInExpr name a x) regions fields
    Src.Record fields   -> List.foldl (\a (_, x) -> varInExpr name a x) regions fields
    Src.Unit            -> regions
    Src.Tuple a b cs    -> List.foldl (varInExpr name) regions (a : b : cs)
    Src.Shader _ _      -> regions


varQualInExpr :: Name -> Name -> [A.Region] -> Src.Expr -> [A.Region]
varQualInExpr p n rs (A.At r e) =
  case e of
    Src.Chr _                 -> rs
    Src.Str _                 -> rs
    Src.Int _                 -> rs
    Src.Float _               -> rs
    Src.Var _ varName         -> rs
    Src.VarQual _ prefix name -> if prefix == p && name == n then r : rs else rs
    Src.List exprs            -> List.foldl (varQualInExpr p n) rs exprs
    Src.Op _                  -> rs
    Src.Negate expr           -> varQualInExpr p n rs expr

    Src.Binops ops final ->
      List.foldl (\a (op, _) -> varQualInExpr p n a op)
        (varQualInExpr p n rs final)
        ops

    Src.Lambda _ body  -> varQualInExpr p n rs body
    Src.Call func args -> List.foldl (varQualInExpr p n) (varQualInExpr p n rs func) args

    Src.If branches finally ->
      List.foldl
        (\rs1 (condition, branch) ->
            varQualInExpr p n (varQualInExpr p n rs1 condition) branch
        )
        (varQualInExpr p n rs finally)
        branches

    Src.Let defs expr ->
      List.foldl
        (\rs1 (A.At _ def_) ->
          case def_ of
            Src.Define _ _ body _ -> varQualInExpr p n rs1 body
            Src.Destruct _ body   -> varQualInExpr p n rs1 body
        )
        (varQualInExpr p n rs expr)
        defs

    Src.Case expr branches ->
      List.foldl (\rs1 (_, expr1) -> varQualInExpr p n rs1 expr1 )
        (varQualInExpr p n rs expr)
        branches

    Src.Accessor _      -> rs
    Src.Access record _ -> varQualInExpr p n rs record

    Src.Update _ fields ->
      List.foldl
        (\rs1 (_, field) -> varQualInExpr p n rs1 field)
        rs
        fields

    Src.Record fields ->
        List.foldl (\rs1 (_, field) -> varQualInExpr p n rs field) rs fields

    Src.Unit         -> rs
    Src.Tuple a b cs -> List.foldl (varQualInExpr p n) rs (a : b : cs)
    Src.Shader _ _   -> rs


infixInModule :: Name -> Src.Module -> [A.Region]
infixInModule name srcMod@(Src.Module _ _ _ imports values _ _ _ _) =
    List.concatMap
      (\(A.At _ (Src.Value _ _ expr _)) ->
        infixInExpr name [] expr
      )
      values


infixInExpr :: Name -> [A.Region] -> Src.Expr -> [A.Region]
infixInExpr n rs (A.At r e) =
  case e of
    Src.Chr _         -> rs
    Src.Str _         -> rs
    Src.Int _         -> rs
    Src.Float _       -> rs
    Src.Var _ _       -> rs
    Src.VarQual _ _ _ -> rs
    Src.List exprs    -> List.foldl (infixInExpr n) rs exprs
    Src.Op opName     -> if opName == n then r : rs else rs
    Src.Negate expr   -> infixInExpr  n rs expr

    Src.Binops exprsAndNames expr ->
      List.foldl
        (\rs1 (expr_, A.At region name) ->
          if n == name then
            infixInExpr n (region : rs1) expr_
          else
            infixInExpr n rs1 expr_
        )
        (infixInExpr n rs expr)
        exprsAndNames

    Src.Lambda patterns expr -> infixInExpr n rs expr

    Src.Call expr exprs ->
      List.foldl (infixInExpr n) (infixInExpr n rs expr) exprs

    Src.If branches finally ->
      List.foldl
        (\rs1 (one, two) ->
          infixInExpr n (infixInExpr n rs1 one) two
        )
        (infixInExpr n rs finally)
        branches

    Src.Let defs expr ->
      List.foldl
        (\rs1 (A.At _ def_) ->
          case def_ of
            Src.Define (A.At _ name_) _ expr_ _ -> infixInExpr n rs1 expr_
            Src.Destruct pattern expr_ -> infixInExpr n rs1 expr_
        )
        (infixInExpr n rs expr)
        defs

    Src.Case expr branches ->
      List.foldl (\rs1 (_, expr1) -> infixInExpr n (infixInExpr n rs1 expr1) expr)
        (infixInExpr n rs expr)
        branches

    Src.Accessor _      -> rs
    Src.Access record _ -> infixInExpr n rs record

    Src.Update _ fields ->
      List.foldl (\rs1 (_, field) -> infixInExpr n rs1 field) rs fields

    Src.Record fields ->
      List.foldl (\rs1 (_, field) -> infixInExpr n rs1 field) rs fields

    Src.Unit         -> rs
    Src.Tuple a b cs -> List.foldl (infixInExpr n) rs (a : b : cs)
    Src.Shader _ _   -> rs


findQualNameInModuleTypes :: ModuleName.Raw -> Name -> Src.Module -> [A.Region]
findQualNameInModuleTypes qual name src =
  concatMap
    (\(A.At _  (Src.Value _ _ expr maybeTipe)) ->
      maybe [] (findQualNameInType qual name . A.toValue) maybeTipe
      ++
      findTypeInExpr (findQualNameInType qual name) expr
    )
    (Src._values src)
  ++
  concatMap
   (\(A.At _ (Src.Union _ _ variants)) ->
     concatMap (concatMap (findQualNameInType qual name . A.toValue) . snd) variants
   )
   (Src._unions src)
  ++
  concatMap
   (\(A.At _ (Src.Alias _ _ tipe)) -> findQualNameInType qual name (A.toValue tipe))
   (Src._aliases src)


findQualNameInType :: ModuleName.Raw -> Name -> Src.Type_ -> [A.Region]
findQualNameInType p n tipe =
  case tipe of
    Src.TLambda arg ret ->
      findQualNameInType p n (A.toValue arg) ++ findQualNameInType p n (A.toValue ret)

    Src.TVar name ->
      []

    Src.TType region _ tlist ->
      concatMap (findQualNameInType p n . A.toValue) tlist

    Src.TTypeQual region prefix name tlist ->
      if prefix == p && name == n then
        region : concatMap (findQualNameInType p n . A.toValue) tlist
      else
        concatMap (findQualNameInType p n . A.toValue) tlist

    Src.TRecord fields extRecord ->
      concatMap (\a -> findQualNameInType p n (A.toValue (snd a))) fields

    Src.TUnit ->
      []

    Src.TTuple a b rest ->
      findQualNameInType p n (A.toValue a)
      ++ findQualNameInType p n (A.toValue b)
      ++ concatMap (findQualNameInType p n . A.toValue) rest


findNameInModuleTypes :: Name -> Src.Module -> [A.Region]
findNameInModuleTypes name src =
  concatMap
    (\(A.At _  (Src.Value _ _ expr maybeTipe)) ->
      maybe [] (findNameInType name . A.toValue) maybeTipe
      ++
      findTypeInExpr (findNameInType name) expr
    )
    (Src._values src)
  ++
  concatMap
   (\(A.At _ (Src.Union _ _ variants)) ->
     concatMap (concatMap (findNameInType name . A.toValue) . snd) variants
   )
   (Src._unions src)
  ++
  concatMap
   (\(A.At _ (Src.Alias _ _ tipe)) -> findNameInType name (A.toValue tipe))
   (Src._aliases src)


findNameInType :: Name -> Src.Type_ -> [A.Region]
findNameInType n tipe =
  case tipe of
    Src.TLambda arg ret ->
      findNameInType n (A.toValue arg) ++ findNameInType n (A.toValue ret)

    Src.TVar name ->
      []

    Src.TType region name tlist ->
      if name == n then
        region : concatMap (findNameInType n . A.toValue) tlist
      else
        concatMap (findNameInType n . A.toValue) tlist

    Src.TTypeQual _ _ _ tlist ->
      concatMap (findNameInType n . A.toValue) tlist

    Src.TRecord fields extRecord ->
      concatMap (\a -> findNameInType n $ A.toValue $ snd a) fields

    Src.TUnit ->
      []

    Src.TTuple a b rest ->
      findNameInType n (A.toValue a)
      ++ findNameInType n (A.toValue b)
      ++ concatMap (findNameInType n . A.toValue) rest


findTypeInExpr :: (Src.Type_ -> [A.Region]) -> Src.Expr -> [A.Region]
findTypeInExpr f expr =
  findTypeInExprHelp f [] expr


findTypeInExprHelp :: (Src.Type_ -> [A.Region]) -> [A.Region] -> Src.Expr -> [A.Region]
findTypeInExprHelp f found (A.At _ e) =
    case e of
        Src.Chr _ -> found
        Src.Str _ -> found
        Src.Int _ -> found
        Src.Float _ -> found
        Src.Var _ varName -> found
        Src.VarQual _ _ _ -> found
        Src.List exprs -> List.foldl (findTypeInExprHelp f) found exprs
        Src.Op _ -> found
        Src.Negate expr -> findTypeInExprHelp f found expr
        Src.Binops exprsAndNames expr ->
          List.foldl
            (\acc (expr_, _) -> findTypeInExprHelp f acc expr_)
            (findTypeInExprHelp f found expr)
            exprsAndNames
        Src.Lambda patterns expr -> findTypeInExprHelp f found expr
        Src.Call expr exprs ->
          List.foldl
            (findTypeInExprHelp f)
            (findTypeInExprHelp f found expr)
            exprs
        Src.If listTupleExprs expr ->
          List.foldl
            (\acc (one, two) ->
              findTypeInExprHelp f (findTypeInExprHelp f acc one) two
            )
            (findTypeInExprHelp f found expr)
            listTupleExprs
        Src.Let defs expr ->
          foldl
            (\acc a ->
              case A.toValue a of
                Src.Define _ _ expr1 (Just (A.At _ tipe)) ->
                  f tipe ++ findTypeInExprHelp f acc expr1

                Src.Define _ _ expr1 _ ->
                  findTypeInExprHelp f acc expr1

                Src.Destruct _ expr1 ->
                  findTypeInExprHelp f acc expr1
            )
            (findTypeInExprHelp f found expr)
            defs

        Src.Case expr branches ->
          List.foldl
            (\acc (pattern, branchExpr) ->
              findTypeInExprHelp f acc branchExpr
            )
            (findTypeInExprHelp f found expr)
            branches
        Src.Accessor _ -> found
        Src.Access expr _ -> findTypeInExprHelp f found expr
        Src.Update _ fields ->
            List.foldl (\acc (_, a) -> findTypeInExprHelp f acc a) found fields
        Src.Record fields ->
            List.foldl (\acc (_, a) -> findTypeInExprHelp f acc a) found fields
        Src.Unit -> found
        Src.Tuple exprA exprB exprs ->
            foldl (findTypeInExprHelp f) found (exprA : exprB : exprs)
        Src.Shader _ _ -> found



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



-- DIAGNOSTICS


data DiagnosticsExit
  = DiagnosticsExitNoRoot
  | DiagnosticsExitBadDetails Reporting.Exit.Details
  | DiagnosticsExitBadBuild Reporting.Exit.BuildProblem


diagnosticsExitToReport :: DiagnosticsExit -> Reporting.Exit.Help.Report
diagnosticsExitToReport exit =
  case exit of
    DiagnosticsExitNoRoot ->
      Reporting.Exit.Help.report "DIAGNOSTICS FOR WHAT?" Nothing
        "I cannot find an elm.json so I am not sure what you want diagnostics for."
        [ Reporting.Doc.reflow $
            "Elm packages always have an elm.json that says current the version number. If\
            \ you run this command from a directory with an elm.json file, I will try to bump\
            \ the version in there based on the API changes."
        ]
    DiagnosticsExitBadDetails details ->
      Reporting.Exit.toDetailsReport details

    DiagnosticsExitBadBuild problem ->
      Reporting.Exit.toBuildProblemReport problem




diagnostics ::
  Reporting.Style
  -> State
  -> FilePath
  -> IO (Either DiagnosticsExit [(FilePath, Int, [Reporting.Report.Report])])
diagnostics style state filePath =
  do  maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Just root -> diagnosticsHelp style root state filePath
        Nothing   -> return $ Left DiagnosticsExitNoRoot


diagnosticsHelp ::
  Reporting.Style ->
  FilePath ->
  State ->
  FilePath ->
  IO (Either DiagnosticsExit [(FilePath, Int, [Reporting.Report.Report])])
diagnosticsHelp style root state filePath =
  do  files <- findElmFilesInSourceDirs root
                 & fmap (filter (\a -> a /= filePath))

      result <-
        Dir.withCurrentDirectory root $
          BW.withScope $ \scope -> Stuff.withRootLock root $
            Task.run $
              do  details <- Task.eio DiagnosticsExitBadDetails $
                               Details.load style scope root

                  buildResult <- Task.eio DiagnosticsExitBadBuild $
                    Build.fromPaths style
                      root
                      details
                      (Data.NonEmptyList.List filePath files)

                  return (buildResult, details)


      case result of
        Right (artifacts, details) -> do
           return $ Right []

        Left (DiagnosticsExitBadBuild buildProblem) -> do
          case Reporting.Exit.toBuildProblemReport buildProblem of
            (Reporting.Exit.Help.CompilerReport _ e es) ->
              return $ Right $ map
                (\(Reporting.Error.Module name path _ source err) ->
                  (
                    path,
                    1,
                    Data.NonEmptyList.toList $
                      Reporting.Error.toReportsForLs (Code.toSource source) err
                  )
                )
                (e : es)

            _ ->
              return $ Left $ DiagnosticsExitBadBuild buildProblem

        Left exit  ->
              return $ Left exit


showArtifacts artifacts =
  do  x <- mapM showModule (Build._modules artifacts)

      return ("_name: " ++ Pkg.toChars (Build._name artifacts) ++
       "\n_artifacts: { _name: " ++ Pkg.toChars (Build._name artifacts) ++
       -- ", _deps: [" ++ List.intercalate ", " (map showDep (Map.toList (Build._deps artifacts))) ++ "]" ++
       -- ", _roots: [" ++ List.intercalate ", " (map showRoot (Data.NonEmptyList.toList (Build._roots artifacts))) ++ "]" ++
       ", _modules: [" ++ List.intercalate ", " (x) ++ "]" ++
       " }")

-- showDep :: (ModuleName.Canonical, I.DependencyInterface) -> String
showDep (ModuleName.Canonical pkg name, _) = Pkg.toChars pkg ++ " - " ++ ModuleName.toChars name

showRoot :: Build.Root -> String
showRoot root = case root of
  Build.Inside name -> "Inside(" ++ ModuleName.toChars name ++ ")"
  Build.Outside name _ _ -> "Outside(" ++ ModuleName.toChars name ++ ")"

showModule :: Build.Module -> IO String
showModule m = case m of
  Build.Fresh name interface _ ->
    return $
      "Fresh(" ++ ModuleName.toChars name ++ ")" ++ "\n" ++
      "  interface: \n" ++
      "    _values: [" ++
        List.intercalate ", " (map (\(a, _) -> Name.toChars a) (Map.toList (Interface._values interface))) ++
      "]" ++ "\n" ++
      "\n"

  Build.Cached name _ cached ->
    do  interface <- Control.Concurrent.MVar.readMVar cached
        case interface of
          Build.Unneeded ->
            return $ "Cached(" ++ ModuleName.toChars name ++ "): " ++ "Unneeded\n"
          Build.Loaded interface_ ->
            return $ "Cached(" ++ ModuleName.toChars name ++ ")\n" ++
              "  interface: \n" ++
              "    _values: [" ++
                List.intercalate ", " (map (\(a, _) -> Name.toChars a) (Map.toList (Interface._values interface_))) ++
              "]" ++ "\n"

          Build.Corrupted ->
            return $ "Cached(" ++ ModuleName.toChars name ++ "): " ++ "Corrupted\n"


findFilesRecursive :: FilePath -> IO [FilePath]
findFilesRecursive dir = do
    entries <- Dir.listDirectory dir
    paths <- mapM (\entry -> do
        let path = dir Path.</> entry
        isDir <- Dir.doesDirectoryExist path
        if isDir
            then findFilesRecursive path
            else return [path]
        ) entries
    return (concat paths)


findElmFilesInSourceDirs :: FilePath -> IO [FilePath]
findElmFilesInSourceDirs root = do
    eitherOutline <- Outline.read root
    case eitherOutline of
      Left _ -> return []
      Right outline ->
        do  srcDirs <- sourceDirs root outline

            fmap concat $
              mapM
                (\(AbsoluteSrcDir srcDir) ->
                  do  files <- findFilesRecursive (root Path.</> srcDir)
                      return $ filter (\f -> Path.takeExtension f == ".elm") files
                )
                srcDirs


sourceDirs :: FilePath -> Outline.Outline -> IO [AbsoluteSrcDir]
sourceDirs root outline =
  case outline of
    Outline.App app ->
      do  srcDirs <- traverse (toAbsoluteSrcDir root)
                       (Data.NonEmptyList.toList (Outline._app_source_dirs app))
          return $ srcDirs

    Outline.Pkg pkg ->
      do  srcDir <- toAbsoluteSrcDir root (Outline.RelativeSrcDir "src")
          return [srcDir]



-- SOURCE DIRECTORY


newtype AbsoluteSrcDir =
  AbsoluteSrcDir FilePath


toAbsoluteSrcDir :: FilePath -> Outline.SrcDir -> IO AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
  AbsoluteSrcDir <$> Dir.canonicalizePath
    (
      case srcDir of
        Outline.AbsoluteSrcDir dir -> dir
        Outline.RelativeSrcDir dir -> root </> dir
    )


addRelative :: AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
  srcDir </> path



-- SYMBOLS


getSymbols ::
  Reporting.Style
  -> State
  -> FilePath
  -> IO (Either DefinitionExit [SymbolInfo])
getSymbols style state filePath =
  Task.run $
    do  root <- Task.mio DefinitionExitNoRoot $
           Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
        details <- Task.eio DefinitionExitBadDetails $
          BW.withScope $ \scope ->
            Stuff.withRootLock root (Details.load style scope root)

        getSymbolsHelp details root state filePath


getSymbolsHelp ::
  Details.Details
  -> FilePath
  -> State
  -> FilePath
  -> Task.Task DefinitionExit [SymbolInfo]
getSymbolsHelp details root state filePath =
  Task.eio id $ LsReporting.trackDocumentSymbol $ Task.run $
  do  src <-
        case Details._outline details of
          Details.ValidApp _ -> loadSrcModuleByPath state filePath
          Details.ValidPkg name _ _ -> loadPkgModuleByPath state name filePath

      let moduleChildren =
            List.concat
              [
                map
                  (\(A.At region (Src.Value (A.At nameRegion name) _ _ _)) ->
                    SymbolInfo name 12 region nameRegion []
                  )
                  (Src._values src),
                map
                  (\(A.At region (Src.Union (A.At nameRegion name) _ variants)) ->
                    SymbolInfo name 10 region nameRegion (
                      map
                        (\(a, type_) ->
                          SymbolInfo (A.toValue a) 22 (A.toRegion a) (A.toRegion a) []
                        )
                        variants
                    )
                  )
                  (Src._unions src),
                map
                  (\(A.At region (Src.Alias (A.At nameRegion name) _ _)) ->
                    SymbolInfo name 23 region nameRegion []
                  )
                  (Src._aliases src),
                map
                  (\(A.At region (Src.Infix name _ _ _)) ->
                    SymbolInfo name 25 region region []
                  )
                  (Src._binops src)
              ]
              & List.sortOn (\a -> let (A.Region (A.Position start _) _) = _symbol_region a in start)

      return $
        maybe
          moduleChildren
          (\name ->
            [
              SymbolInfo
                (A.toValue name)
                2
                (A.toRegion name)
                (A.toRegion name)
                moduleChildren
            ]
          )
          (Src._name src)


data SymbolInfo = SymbolInfo
  { _symbol_name :: Name
  , _symbol_kind :: Int
  , _symbol_region :: A.Region
  , _symbol_selection_region :: A.Region
  , _symbol_children :: [SymbolInfo]
  }


symbolInfoToJson :: SymbolInfo -> Aeson.Value
symbolInfoToJson sym =
  Aeson.object
    [ "name" Aeson..= (Name.toChars (_symbol_name sym) :: String)
    , "range" Aeson..= encodeRange (_symbol_region sym)
    , "selectionRange" Aeson..= encodeRange (_symbol_selection_region sym)
    , "kind" Aeson..= (_symbol_kind sym :: Int)
    , "children" Aeson..= map symbolInfoToJson (_symbol_children sym)
    ]
