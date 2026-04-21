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


data State = State
  { _changedFiles :: Control.Concurrent.MVar.MVar (Map.Map FilePath BS.ByteString)
  , _prevPublishedDiagnosticsFiles :: Control.Concurrent.MVar.MVar [FilePath]
  , _elmFormat :: Control.Concurrent.MVar.MVar (Maybe FilePath)
  }


run :: IO ()
run = do
  state <-
    State
      <$> Control.Concurrent.MVar.newMVar Map.empty
      <*> Control.Concurrent.MVar.newMVar []
      <*> Control.Concurrent.MVar.newMVar Nothing

  loop state

  where
    loop state =
      do  contentLength <- readHeader
          body <- BSLC.hGet IO.stdin (contentLength + 2)
          startTime <- Data.Time.getCurrentTime

          case Aeson.parseEither (\obj -> obj .: "method") =<< Aeson.eitherDecode body of
            Left err ->
              do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                  IO.hFlush IO.stderr
                  loop state

            Right "initialize" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id"
                              rootPath <- params .: "rootPath" :: Aeson.Parser String
                              initializationOptions <- params Aeson..:? "initializationOptions" :: Aeson.Parser (Maybe Aeson.Object)

                              languageServer <- maybe (pure Nothing) (\obj -> obj Aeson..:? "whiletruu-elm-language-server") initializationOptions
                              elmFormatPath <- maybe (pure Nothing) (\obj -> obj Aeson..:? "elmFormatPath") languageServer

                              return ( id, rootPath, elmFormatPath)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                          IO.hFlush IO.stderr
                          loop state

                    Right (id, rootPath, elmFormatPath) ->
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
                                    , "version" .= ("0.0.1" :: String)
                                    ]
                                  ]

                          respond id response

                          loop state

            Right "initialized" ->
              do  loop state

            Right "shutdown" ->
              do  let result = Aeson.parseEither (\obj -> obj .: "id") =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr $ "Error decoding JSON: " ++ err
                          IO.hFlush IO.stderr
                          loop state

                    Right id ->
                      do  respond id Aeson.Null
                          loop state

            Right "exit" ->
              Exit.exitSuccess

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
                      do  Control.Concurrent.MVar.modifyMVar_ (_changedFiles state) $ \a ->
                            return $ Map.insert filePath (BS_UTF8.fromString text) a

                          result <- diagnostics filePath []

                          case result of
                            Left err ->
                              do  showMessage MessageTypeError $ Reporting.Exit.toString $
                                    diagnosticsExitToReport err

                                  loop state

                            Right stuffs ->
                              do  Control.Concurrent.MVar.modifyMVar_ (_prevPublishedDiagnosticsFiles state) $
                                    \prev ->
                                      do  let diff = List.filter (\a -> List.all (\(n, _, _) -> n /= a) stuffs) prev
                                          mapM_ (\a -> publishReportDiagnostic a 1 []) diff

                                          return (map (\(a,_,_) -> a) stuffs)

                                  mapM_
                                    (\(filePath, i, reports) ->
                                      publishReportDiagnostic filePath i reports
                                    )
                                    stuffs

                                  loop state

            Right "textDocument/didSave" ->
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

                          result <- diagnostics filePath []

                          case result of
                            Left err ->
                              do  showMessage MessageTypeError $ Reporting.Exit.toString $
                                    diagnosticsExitToReport err

                                  loop state

                            Right stuffs ->
                              do  prev <- Control.Concurrent.MVar.readMVar (_prevPublishedDiagnosticsFiles state)

                                  let diff = List.filter (\a -> List.all (\(n, _, _) -> n /= a) stuffs) prev

                                  mapM_ (\a -> publishReportDiagnostic a 1 []) diff

                                  Control.Concurrent.MVar.modifyMVar_ (_prevPublishedDiagnosticsFiles state)
                                    (\a -> pure (map (\(a,_,_) -> a) stuffs))

                                  mapM_
                                    (\(filePath, i, reports) ->
                                      publishReportDiagnostic filePath i reports
                                    )
                                    stuffs

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

                              changes <-
                                mapM parseTextDocumentContentChangeEvent =<<
                                  params .: "contentChanges" :: Aeson.Parser [((A.Position, A.Position), String)]

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
                      do  let workDoneToken = "goto-definition"
                          sendCreateWorkDoneProgress workDoneToken
                          sendProgressBegin workDoneToken "👀 Finding definition"

                          result <- findDefinition state filePath position
                          endTime <- Data.Time.getCurrentTime

                          sendProgressEnd workDoneToken $
                             "Done in " ++ show (Data.Time.diffUTCTime endTime startTime)

                          case result of
                            Right (definitionFilePath, _, _, A.At region _) ->
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
                      do  let workDoneToken = "goto-references"
                          sendCreateWorkDoneProgress workDoneToken
                          sendProgressBegin workDoneToken "🔍 Finding references"

                          result <- findReferences state filePath position
                          endTime <- Data.Time.getCurrentTime
                          let timeDiff = Data.Time.diffUTCTime endTime startTime

                          case result of
                            Right references ->
                              do  let amount = length (concat (Map.elems references))
                                  sendProgressEnd workDoneToken $
                                    "Found " ++ show amount ++ " in " ++ show timeDiff

                                  respond id
                                    $ Aeson.toJSON
                                    $ concatMap (\(fp, regions) ->
                                        map (encodeRegion fp) regions
                                      ) (Map.toList references)
                                  loop state

                            Left err ->
                              do  sendProgressEnd workDoneToken $
                                    "Failed to find references in " ++ show timeDiff

                                  respondErr id $ Reporting.Exit.toString $
                                    definitionExitToReport filePath err
                                  loop state

            Right "textDocument/prepareRename" ->
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
                      do  maybeTarget <- findDefinition state filePath position
                          case maybeTarget of
                            Right (_, _, element, _) ->
                              do  respond id $
                                    Aeson.object
                                      [ "range" .= encodeRange (A.toRegion element)
                                      , "placeholder" .=  (elementToRenamePlaceholder element :: String)
                                      ]
                                  loop state

                            Left _ ->
                              do  respond id Aeson.Null
                                  loop state

            Right "textDocument/rename" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id" :: Aeson.Parser Int

                              textDocument <- params .: "textDocument"
                              uri <- textDocument .: "uri" :: Aeson.Parser String

                              position <- params .: "position"
                              row <- position .: "line" :: Aeson.Parser Int
                              column <- position .: "character" :: Aeson.Parser Int
                              newName <- params .: "newName" :: Aeson.Parser String

                              let filePath = drop 7 uri
                              let position = A.Position
                                               (fromIntegral row + 1)
                                               (fromIntegral column + 1)

                              return (id, filePath, position, newName)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right (id, filePath, position, newName) ->
                      do  let workDoneToken = "rename"
                          sendCreateWorkDoneProgress workDoneToken
                          sendProgressBegin workDoneToken "✏️ Renaming"

                          result <- findReferences state filePath position
                          endTime <- Data.Time.getCurrentTime
                          let timeDiff = Data.Time.diffUTCTime endTime startTime

                          case result of
                            Right references ->
                              do  let amount = length (concat (Map.elems references))
                                  sendProgressEnd workDoneToken $
                                    "Found " ++ show amount ++ " in " ++ show timeDiff

                                  respond id $ Aeson.toJSON $
                                     encodeWorkspaceEdit references (Name.fromChars newName)
                                  loop state

                            Left err ->
                              do  sendProgressEnd workDoneToken $
                                    "Failed to rename in " ++ show timeDiff

                                  respondErr id $ Reporting.Exit.toString $
                                    definitionExitToReport filePath err
                                  loop state

            Right "textDocument/formatting" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id" :: Aeson.Parser Int

                              textDocument <- params .: "textDocument"
                              uri <- textDocument .: "uri" :: Aeson.Parser String

                              let filePath = drop 7 uri

                              pure (id, filePath)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right (id, filePath) ->
                      do  elmFormat <- Control.Concurrent.MVar.readMVar (_elmFormat state)

                          case elmFormat of
                            Nothing ->
                              do  respondErr id "elm-format not found"
                                  loop state

                            Just executable ->
                              do  files <- Control.Concurrent.MVar.readMVar (_changedFiles state)
                                  source <- maybe (File.readUtf8 filePath) return $ Map.lookup filePath files

                                  result <-
                                    Exception.try $
                                      Proc.readCreateProcessWithExitCode (Proc.proc executable ["--stdin"]) (BS_UTF8.toString source)
                                      :: IO (Either IOError (Exit.ExitCode, String, String))

                                  case result of
                                    Left _ ->
                                      do  respondErr id $ "Failed to run elm-format: " ++ executable
                                          loop state

                                    Right (Exit.ExitFailure _, _, stderr_ )->
                                      do  respondErr id stderr_
                                          loop state

                                    Right (Exit.ExitSuccess, stdout_, _)->
                                      let
                                        lines = BSC.split '\n' source
                                      in
                                      do  respond id $ Aeson.toJSON
                                            [ Aeson.object
                                                [ "range" .= Aeson.object
                                                  [ "start" .= Aeson.object
                                                      [ "line" .= (0 :: Int)
                                                      , "character" .= (0 :: Int)
                                                      ]
                                                  , "end" .= Aeson.object
                                                      [ "line" .= max 0 (length lines - 1)
                                                      , "character" .= case reverse lines of
                                                                         last : _ -> BSC.length last
                                                                         [] -> 0
                                                      ]
                                                  ]
                                                , "newText" .= stdout_
                                                ]
                                            ]
                                          loop state

            Right "textDocument/documentSymbol" ->
              do  let result =
                        Aeson.parseEither (\obj ->
                          do  params <- obj .: "params"
                              id <- obj .: "id" :: Aeson.Parser Int

                              textDocument <- params .: "textDocument"
                              uri <- textDocument .: "uri" :: Aeson.Parser String

                              let filePath = drop 7 uri

                              return (id, filePath)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err ->
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop state

                    Right (id, filePath) ->
                      do  let workDoneToken = "document-symbols"
                          sendCreateWorkDoneProgress workDoneToken
                          sendProgressBegin workDoneToken "Finding symbols"

                          result <- singleFileForSymbols state filePath
                          endTime <- Data.Time.getCurrentTime
                          let timeDiff = Data.Time.diffUTCTime endTime startTime

                          case result of
                            Right singleFile ->
                              do  case _source singleFile of
                                    Right srcMod ->
                                      do  sendProgressEnd workDoneToken $
                                            "Got result in " ++ show timeDiff

                                          respond id $ Aeson.toJSON $ srcModuleSymbols srcMod

                                          loop state

                                    Left err ->
                                      do  sendProgressEnd workDoneToken $
                                            "Failed in " ++ show timeDiff

                                          loop state
                            Left err ->
                              do  sendProgressEnd workDoneToken $
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

    DefinitionExitNoFile path ->
      Reporting.Exit.Help.report "NO FILE" Nothing
        ("I tried to load a file from " ++ path ++ ", but it does not exist.")
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
  | FoundPattern Src.Pattern_
  | FoundDef Src.Def
  | FoundInfix Src.Infix
  | FoundAlias Src.Alias
  | FoundUnion Src.Union
  | FoundVariant (A.Located Name, [Src.Type])
  | FoundModuleName Name


findDefinition :: State -> FilePath -> A.Position -> IO (Either DefinitionExit (FilePath, Src.Module, Element, Found))
findDefinition state filePath position =
  do  maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Just root -> findDefinitionHelp root state filePath position
        Nothing   -> return $ Left DefinitionExitNoRoot


findDefinitionHelp ::
  FilePath
  -> State
  -> FilePath
  -> A.Position
  -> IO (Either DefinitionExit (FilePath, Src.Module, Element, Found))
findDefinitionHelp root state filePath position =
  BW.withScope $ \scope ->
  Stuff.withRootLock root $ Task.run $
  do  details <- Task.eio DefinitionExitBadDetails $ Details.load Reporting.silent scope root

      src <-
        case Details._outline details of
          Details.ValidApp _ -> loadSrcModuleByPath state filePath
          Details.ValidPkg name _ _ -> loadPkgModuleByPath state name filePath

      case findElement position src of
        Just element ->
          Task.eio id $
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
      EVar defs patterns Src.LowVar name ->
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
                (Nothing, Right (Just a)) -> Right $ (\(a, b, c) -> (a, b, fmap FoundValue c)) a
                (Nothing, Left a) -> Left a
                (Nothing, Right Nothing) -> Left (DefinitionExitNotFound element)

      EVarQual _ _ Src.LowVar mod name ->
       fmap
         (\a -> a
           >>= fmap (\(a, b, c) -> (a, b, fmap FoundValue c)) . maybe (Left (DefinitionExitNotFound element)) Right
         )
         (findDefinitionForLowVarQualInImports state details root (Src._imports src) mod name)

      EVar _ _ Src.CapVar name ->
        do  let local = findDefinitionForCapVarLocally src name
            external <- findDefinitionForCapVarInImports state details root (Src._imports src) name

            return $
              case (local, external) of
                (Just a, _) -> Right (path, src, a)
                (Nothing, Right (Just a)) -> Right a
                (Nothing, Left a) -> Left a
                (Nothing, Right Nothing) -> Left (DefinitionExitNotFound element)

      EVarQual _ _ Src.CapVar mod name ->
         findDefinitionForCapVarQualInImports state details root (Src._imports src) mod name
         & fmap (\a -> a >>= maybe (Left (DefinitionExitNotFound element)) Right)

      EInfix name_ ->
        fmap
          (\a -> a
            >>= fmap (\(a, b, c) -> (a, b, fmap FoundInfix c)) . maybe (Left (DefinitionExitNotFound element)) Right
          )
          (findDefinitionForInfixInImports state details root (Src._imports src) name_)

      EModuleName name_ ->
        fmap
          (\a -> a
            >>= fmap (\(a, b, c) -> (a, b, fmap FoundModuleName c)) . maybe (Left (DefinitionExitNotFound element)) Right
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


findDefinitionForCapVarLocally :: Src.Module -> Name -> Maybe Found
findDefinitionForCapVarLocally src name =
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
            else
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


findDefinitionForCapVarInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, Found)))
findDefinitionForCapVarInImports state details root imports name =
  let
    potentialSources =
      filter
        (\import_@(Src.Import iName iAlias iExposing) ->
          case iExposing of
            Src.Open -> True
            Src.Explicit exposed -> any (\exposed ->
                case exposed of
                  Src.Lower _ -> False
                  Src.Upper (A.At _ name_) Src.Private -> name_ == name
                  Src.Upper _ _ -> True
                  Src.Operator _ _ -> False
              )
              exposed
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForCapVarInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, x) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    potentialSources


findDefinitionForCapVarInModule ::
  State
  -> Details.Details
  -> FilePath
  -> ModuleName.Raw
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, Found)))
findDefinitionForCapVarInModule state details root moduleName name =
  do  pathAndSrc <- loadSrcModule state details root moduleName

      case pathAndSrc of
        Right (path, src) ->
          return $ Right $ fmap (\a -> (path, src, a)) $ findDefinitionForCapVarLocally src name

        Left exit ->
          return $ Left exit


findDefinitionForCapVarQualInImports ::
  State
  -> Details.Details
  -> FilePath
  -> [Src.Import]
  -> Name
  -> Name
  -> IO (Either DefinitionExit (Maybe (FilePath, Src.Module, Found)))
findDefinitionForCapVarQualInImports state details root imports qual name =
  let
    potentialSources =
      filter
        (\import_@(Src.Import iName iAlias iExposing) ->
          A.toValue iName == qual || Just qual == fmap A.toValue iAlias
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForCapVarInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, x) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    potentialSources


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
      filter
        (\import_@(Src.Import iName iAlias iExposing) ->
          A.toValue iName == qual || Just qual == fmap A.toValue iAlias
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, x) -> return x
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
            Src.Explicit exposed -> any (\exposed ->
                case exposed of
                  Src.Lower (A.At _ name_) -> name_ == name
                  Src.Upper _ _ -> False
                  Src.Operator _ _ -> False
              )
              exposed
        )
        imports
  in
  foldr
    (\import_ acc ->
      do  x <- findDefinitionForNameInModule state details root (Src.getImportName import_) name
          y <- acc

          case (y, x) of
            (Left _, x) -> return x
            (Right Nothing, _) -> return x
            (Right (Just _), _) -> return y
    )
    (return (Right Nothing))
    potentialSources


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
            Src.Explicit exposed -> any (\exposed ->
                case exposed of
                  Src.Lower _ -> False
                  Src.Upper _ _ -> False
                  Src.Operator _ name_ -> name_ == name
              )
              exposed
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


findDefinitionForNameInPattern :: Name -> Src.Pattern -> Maybe (A.Region, Src.Pattern_)
findDefinitionForNameInPattern name pattern@(A.At _ pattern_) =
  case pattern_ of
    Src.PAnything -> Nothing
    Src.PVar pname ->
      if pname == name then Just (A.toRegion pattern, A.toValue pattern) else Nothing
    Src.PRecord names ->
      foldr (\(A.At loc name_) acc -> if name_ == name then Just (loc, pattern_) else acc) Nothing names
    Src.PAlias aPattern (A.At loc aName) ->
      if aName == name then
        Just (loc, pattern_)
      else
        findDefinitionForNameInPattern name aPattern
    Src.PUnit -> Nothing
    Src.PTuple a b c ->
      foldr (\p acc -> findDefinitionForNameInPattern  name p <|> acc) Nothing (a : b : c)
    Src.PCtor _ _ args ->
      foldr (\p acc -> findDefinitionForNameInPattern name p <|> acc) Nothing args
    Src.PCtorQual _ _ _ _ -> Nothing
    Src.PList patterns ->
      foldr (\p acc -> findDefinitionForNameInPattern name p <|> acc) Nothing patterns
    Src.PCons a b ->
      findDefinitionForNameInPattern name a <|> findDefinitionForNameInPattern name b
    Src.PChr _ -> Nothing
    Src.PStr _ -> Nothing
    Src.PInt _ -> Nothing


type Element = A.Located Element_


data Element_
  = EVar [A.Located Src.Def] [Src.Pattern] Src.VarType Name
  | EVarQual [A.Located Src.Def] [Src.Pattern] Src.VarType Name Name
  | EAccess [A.Located Src.Def] [Src.Pattern] Src.Expr Name
  | EInfix Name
  | EModuleName Name


elementToStr :: Element_ -> String
elementToStr element =
  case element of
    EVar _ _ _ name ->
      Name.toChars name ++ " (Var)"
    EVarQual _ _ _ prefix name ->
      Name.toChars prefix ++ "." ++ Name.toChars name ++ " (VarQual)"
    EAccess _ _ record field ->
      "." ++ Name.toChars field ++ " (Access)"
    EInfix name ->
      Name.toChars name ++ " (Infix)"
    EModuleName name ->
      Name.toChars name ++ " (Module)"


elementToRenamePlaceholder :: Element -> String
elementToRenamePlaceholder element =
  case A.toValue element of
    EVar _ _ _ name ->
      Name.toChars name
    EVarQual _ _ _ prefix name ->
      Name.toChars name
    EAccess _ _ record field ->
      Name.toChars field
    EInfix name ->
      Name.toChars name
    EModuleName name ->
      Name.toChars name


findElement :: A.Position -> Src.Module -> Maybe Element
findElement pos src =
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
                  then Just $ A.At (A.toRegion name) $ EVar [] [] Src.LowVar (A.toValue name)
                  else acc
              Src.Upper name _ ->
                if isInRegion pos (A.toRegion name)
                  then Just $ A.At (A.toRegion name) $ EVar [] [] Src.CapVar (A.toValue name)
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
      let a =
            if isInRegion pos (A.toRegion name)
              then Just (A.At (A.toRegion name) (EVar [] [] Src.CapVar (A.toValue name)))
              else Nothing

          b = findElementInType pos type_
      in
      a <|> b <|> found
    )
    Nothing
    aliases


findElementInUnions :: A.Position -> [A.Located Src.Union] -> Maybe Element
findElementInUnions pos unions =
  foldr
    (\(A.At region (Src.Union name _ variants)) found ->
      let a =
            if isInRegion pos (A.toRegion name)
              then Just (A.At (A.toRegion name) (EVar [] [] Src.CapVar (A.toValue name)))
              else Nothing

          b =
            foldr
              (\(name_, types) found_ ->
                let a_ =
                      if isInRegion pos (A.toRegion name_)
                        then Just (A.At (A.toRegion name_) (EVar [] [] Src.CapVar (A.toValue name_)))
                        else Nothing

                    b_ =
                      foldr (\a acc -> findElementInType pos a <|> acc) Nothing types
                in
                a_ <|> b_ <|> found_

              )
              Nothing
              variants
      in
      a <|> b <|> found
    )
    Nothing
    unions


findElementInImports :: A.Position -> [Src.Import] -> Maybe Element
findElementInImports pos imports =
  foldr
    (\(Src.Import name alias exposing) found ->
      let a =
            if isInRegion pos (A.toRegion name)
              then Just (A.At (A.toRegion name) (EModuleName (A.toValue name)))
              else
                maybe
                  Nothing
                    (\a ->
                      if isInRegion pos (A.toRegion a)
                        then Just (A.At (A.toRegion name) (EModuleName (A.toValue name)))
                        else Nothing
                    )
                    alias

          b =
            case exposing of
              Src.Open ->
                Nothing

              Src.Explicit exposed ->
                foldr
                  (\a acc ->
                    case a of
                      Src.Lower name ->
                        if isInRegion pos (A.toRegion name)
                          then Just (A.At (A.toRegion name) (EVar [] [] Src.CapVar (A.toValue name)))
                          else acc

                      Src.Upper name _ ->
                        if isInRegion pos (A.toRegion name)
                          then Just (A.At (A.toRegion name) (EVar [] [] Src.CapVar (A.toValue name)))
                          else acc

                      Src.Operator region name ->
                        if isInRegion pos region
                          then Just (A.At region (EInfix name))
                          else acc
                  )
                  Nothing
                  exposed
      in
      a <|> b <|> found
    )
    Nothing
    imports


findElementInValues :: A.Position -> [A.Located Src.Value] -> Maybe Element
findElementInValues pos values =
  foldr
    (\located found ->
      case located of
        A.At region (Src.Value name patterns body type_) ->
          let a =
                if isPositionOnValueName pos located
                  then Just (A.At (A.toRegion name) (EVar [] [] Src.LowVar (A.toValue name)))
                else if isInRegion pos region
                  then findElementInExpr pos [] patterns body
                else Nothing

              b = findElementInType pos Control.Monad.=<< type_
          in
          a <|> b <|> found
    )
    Nothing
    values


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
            then Just (A.At region (EVar [] [] Src.CapVar name))
            else foldr (\a acc -> findElementInType pos a <|> acc) Nothing tlist

        Src.TTypeQual region qual name tlist ->
          if isInRegion pos region
            then Just (A.At region (EVarQual [] [] Src.CapVar qual name))
            else foldr (\a acc -> findElementInType pos a <|> acc) Nothing tlist

        Src.TRecord fields extRecord ->
            foldr (\a acc -> findElementInType pos (snd a) <|> acc) Nothing fields

        Src.TUnit ->
          Nothing

        Src.TTuple a b rest ->
            foldr (\a acc -> findElementInType pos a <|> acc) Nothing (a : b : rest)

    else
      Nothing


isPositionOnValueName :: A.Position -> A.Located Src.Value -> Bool
isPositionOnValueName pos value =
    let (A.At (A.Region (A.Position sx sy) _) (Src.Value name _ _ typeAnn)) = value
        valNameLen = fromIntegral (length (Name.toChars (A.toValue name)))
    in
    isInRegion pos (A.toRegion name)
      || (Maybe.isJust typeAnn
           && isInRegion pos (A.Region (A.Position sx 0) (A.Position sx valNameLen))
         )


isInRegion :: A.Position -> A.Region -> Bool
isInRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
    && (row == endRow && col <= endCol || row < endRow)


findElementInExpr
  :: A.Position
  -> [A.Located Src.Def]
  -> [Src.Pattern]
  -> Src.Expr
  -> Maybe Element
findElementInExpr position defs patterns expr@(A.At region _) =
  case A.toValue expr of
    Src.Chr _ ->
      Nothing

    Src.Str _ ->
      Nothing

    Src.Int _ ->
      Nothing

    Src.Float _ ->
      Nothing

    Src.Var varType name ->
      Just $ A.At region $ EVar defs patterns varType name

    Src.VarQual varType prefix name ->
      Just $ A.At region $ EVarQual defs patterns varType prefix name

    Src.List exprs ->
      foldr
        (\a acc ->
          if isInRegion position (A.toRegion a) then
            findElementInExpr position defs patterns a
          else
            acc
        )
        Nothing
        exprs

    Src.Op name ->
      Just $ A.At region $ EInfix name

    Src.Negate expr ->
      if isInRegion position (A.toRegion expr) then
        findElementInExpr position defs patterns expr
      else
        Nothing

    Src.Binops ops final ->
      if isInRegion position (A.toRegion final) then
        findElementInExpr position defs patterns final
      else
        foldr
          (\(expr_, op) acc ->
            if isInRegion position (A.toRegion op) then
              Just $ A.At (A.toRegion op) $ EInfix (A.toValue op)
            else if isInRegion position (A.toRegion expr_) then
              findElementInExpr position defs patterns expr_
            else
              acc
          )
          Nothing
          ops

    Src.Lambda srcArgs body ->
      if isInRegion position (A.toRegion body) then
        findElementInExpr position defs (srcArgs ++ patterns) body
      else
        Nothing

    Src.Call func args ->
      if isInRegion position (A.toRegion func) then
        findElementInExpr position defs patterns func
      else
        foldr
          (\a acc ->
            if isInRegion position (A.toRegion a) then
              findElementInExpr position defs patterns a
            else
              acc
          )
          Nothing
          args

    Src.If branches finally_ ->
      if isInRegion position (A.toRegion finally_) then
        findElementInExpr position defs patterns finally_
      else
        foldr
          (\(condition, branch) acc ->
            if isInRegion position (A.toRegion condition) then
              findElementInExpr position defs patterns condition
            else if isInRegion position (A.toRegion branch) then
              findElementInExpr position defs patterns branch
            else
              acc
          )
          Nothing
          branches

    Src.Let defs1 body ->
      if isInRegion position (A.toRegion body) then
        findElementInExpr position (defs1 ++ defs) patterns body
      else
        foldr
          (\def acc ->
              case (A.toValue def) of
                Src.Define _ patterns1 expr type_ ->
                  let a = if isInRegion position (A.toRegion expr) then
                            findElementInExpr position (def : defs) (patterns1 ++ patterns) expr

                          else
                            acc

                      b = findElementInType position Control.Monad.=<< type_
                  in
                  a <|> b <|> acc

                Src.Destruct pattern expr ->
                  if isInRegion position (A.toRegion expr) then
                    findElementInExpr position (def : defs) (pattern : patterns) expr
                  else
                    acc
          )
          Nothing
          defs1

    Src.Case expr branches ->
      if isInRegion position (A.toRegion expr) then
        findElementInExpr position defs patterns expr
      else
        foldr
          (\(pattern, branch) acc ->
            if isInRegion position (A.toRegion branch) then
              findElementInExpr position defs (pattern : patterns) branch
            else
              acc
          )
          Nothing
          branches

    Src.Accessor field ->
      Nothing

    Src.Access record field ->
      if isInRegion position (A.toRegion field) then
        Just $ A.At (A.toRegion field) $ EAccess defs patterns record (A.toValue field)
      else if isInRegion position (A.toRegion record) then
        findElementInExpr position defs patterns record
      else
        Nothing

    Src.Update starter fields ->
      if isInRegion position (A.toRegion starter) then
        Just $ A.At (A.toRegion starter) $ EVar defs patterns Src.LowVar (A.toValue starter)

      else
        foldr
          (\(field, value) acc ->
            if isInRegion position (A.toRegion field) then
              Just $ A.At (A.toRegion field) $ EVar defs patterns Src.LowVar (A.toValue field)
            else if isInRegion position (A.toRegion value) then
              findElementInExpr position defs patterns value
            else
              acc
          )
          Nothing
          fields

    Src.Record fields ->
      foldr
        (\(field, value) acc ->
          if isInRegion position (A.toRegion value) then
            findElementInExpr position defs patterns value
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
            findElementInExpr position defs patterns expr
          else
            exprs
        )
        Nothing
        (a : b : cs)

    Src.Shader _ _ ->
      Nothing



-- REFERENCES


findReferences :: State -> FilePath -> A.Position -> IO (Either DefinitionExit (Map.Map FilePath [A.Region]))
findReferences state filePath position =
  do  maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot
      case maybeRoot of
        Just root -> findReferencesHelp root state filePath position
        Nothing   -> return $ Left DefinitionExitNoRoot


findReferencesHelp :: FilePath -> State -> FilePath -> A.Position -> IO (Either DefinitionExit (Map.Map FilePath [A.Region]))
findReferencesHelp root state filePath position =
  BW.withScope $ \scope ->
  Stuff.withRootLock root $ Task.run $
  do  details <- Task.eio DefinitionExitBadDetails $ Details.load Reporting.silent scope root

      localSrc <-
        case Details._outline details of
          Details.ValidApp _ -> loadSrcModuleByPath state filePath
          Details.ValidPkg name _ _ -> loadPkgModuleByPath state name filePath

      definition <-
        case findElement position localSrc of
          Just element ->
            Task.eio id $ findDefinitionForElement state details root filePath localSrc (A.toValue element)

          Nothing ->
            Task.throw DefinitionExitNoElement

      case definition of
        (modulePath, defSrc, A.At defRegion (FoundValue value@(Src.Value (A.At _ name) _ _ _))) ->
          let
            local =
              List.concatMap (findVarInValue name . A.toValue) (Src._values defSrc)
              ++ valueRegions (A.At defRegion value)
              ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                    & maybe [] (\a -> [a])
                 )
          in
          Task.io $ foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ ->
                      -- Ignore file not found - importersOf can return
                      -- importers for deleted files since Details are loaded
                      -- from elm-stuff
                      acc

                    Right (path, src) ->
                      let
                        imported =
                          findReferencesForImportedLowVar (Src.getName defSrc) name src
                      in
                      fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

        (modulePath, defSrc, A.At defRegion (FoundAlias value@(Src.Alias (A.At region name) _ _))) ->
          let
            local =
              region : findNameInModuleTypes name defSrc
              ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                   & maybe [] (\a -> [a])
                 )
          in
          Task.io $ foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ ->
                      -- Ignore file not found - importersOf can return
                      -- importers for deleted files since Details are loaded
                      -- from elm-stuff
                      acc

                    Right (path, src) ->
                      let
                        maybeImport =
                          List.find
                            (\a -> A.toValue (Src._import a) == (Src.getName defSrc))
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
                      fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

        (modulePath, defSrc, A.At defRegion (FoundUnion value@(Src.Union (A.At region name) _ _))) ->
          let
            local =
              region : findNameInModuleTypes name defSrc
              ++ (findNameInExposing name (A.toValue (Src._exports defSrc))
                   & maybe [] (\a -> [a])
                 )
          in
          Task.io $ foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ ->
                      -- Ignore file not found - importersOf can return
                      -- importers for deleted files since Details are loaded
                      -- from elm-stuff
                      acc

                    Right (path, src) ->
                      let
                        maybeImport =
                          List.find
                            (\a -> A.toValue (Src._import a) == (Src.getName defSrc))
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
                      fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

        (modulePath, defSrc, A.At defRegion (FoundInfix infix_@(Src.Infix name _ _ _))) ->
          let local = infixInModule name localSrc in
          Task.io $ foldr
            (\a acc ->
              do  loadResult <- loadSrcModule state details root a
                  case loadResult of
                    Left _ ->
                      -- Ignore file not found - importersOf can return
                      -- importers for deleted files since Details are loaded
                      -- from elm-stuff                      let newRefs =
                      acc

                    Right (path, src) ->
                      let
                        imported =
                          case List.find (\a -> A.toValue (Src._import a) == Src.getName defSrc) (Src._imports src) of
                            Just import_@(Src.Import _ alias _) ->
                              if isInfixExposed import_ name then
                                infixInModule name src

                              else
                                []

                            Nothing -> []
                      in
                      fmap (Map.insert path imported) acc
            )
            (return $ Map.singleton modulePath local)
            (importersOf details (Src.getName defSrc))

        _ ->
          return Map.empty


findReferencesForImportedLowVar :: ModuleName.Raw -> Name -> Src.Module -> [A.Region]
findReferencesForImportedLowVar moduleName name src =
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
  if any (Maybe.isJust . findDefinitionForNameInPattern name) patterns
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
varInExpr name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ varName -> if varName == name then region : foundRegions else foundRegions
        Src.VarQual _ qual varName -> foundRegions
        Src.List exprs -> List.foldl (varInExpr name) foundRegions exprs
        Src.Op _ -> foundRegions
        Src.Negate expr -> varInExpr name foundRegions expr
        Src.Binops exprsAndNames expr ->
          List.foldl
            (\foundRegions (expr_, _) -> varInExpr name foundRegions expr_)
            (varInExpr name foundRegions expr)
            exprsAndNames
        Src.Lambda patterns expr ->
          if any (Maybe.isJust . findDefinitionForNameInPattern name) patterns
            then foundRegions
            else varInExpr name foundRegions expr
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
          let
            isNameInDefs =
              any
                (\a ->
                  case A.toValue a of
                    Src.Define (A.At _ name_) _ _ _ -> name == name_
                    Src.Destruct pattern _ ->
                      Maybe.isJust (findDefinitionForNameInPattern name pattern)
                )
              defs
          in
          if isNameInDefs then
            foundRegions
          else
            List.foldl
              (\foundRegions (A.At _ def_) ->
                case def_ of
                  Src.Define (A.At _ name_) _ expr_ _ -> varInExpr name foundRegions expr_
                  Src.Destruct pattern expr_ -> varInExpr name foundRegions expr_
              )
              (varInExpr name foundRegions expr)
              defs
        Src.Case expr branches ->
          if any (Maybe.isJust . findDefinitionForNameInPattern name . fst) branches
            then foundRegions
            else List.foldl
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


varQualInExpr :: Name -> Name -> [A.Region] -> Src.Expr -> [A.Region]
varQualInExpr qual name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ varName -> foundRegions
        Src.VarQual _ qual_ varName ->
          if qual == qual_ && varName == name then region : foundRegions else foundRegions
        Src.List exprs -> List.foldl (varQualInExpr qual name) foundRegions exprs
        Src.Op _ -> foundRegions
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
                (varQualInExpr qual name foundRegions expr)
                defs
        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    varQualInExpr qual name foundRegions branchExpr
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


infixInModule :: Name -> Src.Module -> [A.Region]
infixInModule name srcMod@(Src.Module _ _ _ imports values _ _ _ _) =
    List.concatMap
      (\(A.At _ (Src.Value _ _ expr _)) ->
        infixInExpr name [] expr
      )
      values


infixInExpr :: Name -> [A.Region] -> Src.Expr -> [A.Region]
infixInExpr name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ _ -> foundRegions
        Src.VarQual _ _ _ -> foundRegions
        Src.List exprs -> List.foldl (infixInExpr name) foundRegions exprs
        Src.Op opName -> if opName == name then region : foundRegions else foundRegions
        Src.Negate expr -> infixInExpr  name foundRegions expr
        Src.Binops exprsAndNames expr ->
            List.foldl
                (\foundRegions (expr_, (A.At region name_)) ->
                  if name == name_ then
                    infixInExpr name (region : foundRegions) expr_
                  else
                    infixInExpr name foundRegions expr_
                )
                (infixInExpr name foundRegions expr)
                exprsAndNames
        Src.Lambda patterns expr -> infixInExpr name foundRegions expr
        Src.Call expr exprs -> List.foldl (infixInExpr name) (infixInExpr name foundRegions expr) exprs
        Src.If listTupleExprs expr ->
            List.foldl
                (\foundRegions (one, two) ->
                    infixInExpr name (infixInExpr name foundRegions one) two
                )
                (infixInExpr name foundRegions expr)
                listTupleExprs
        Src.Let defs expr ->
            List.foldl
                (\foundRegions (A.At _ def_) ->
                    case def_ of
                        Src.Define (A.At _ name_) _ expr_ _ ->
                            infixInExpr name foundRegions expr_

                        Src.Destruct pattern expr_ ->
                            infixInExpr name foundRegions expr_
                )
                (infixInExpr name foundRegions expr)
                defs
        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    infixInExpr name (infixInExpr name foundRegions branchExpr) expr
                )
                (infixInExpr name foundRegions expr)
                branches
        Src.Accessor _ -> foundRegions
        Src.Access expr _ -> infixInExpr name foundRegions expr
        Src.Update _ fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> infixInExpr name foundRegions fieldExpr)
                foundRegions
                fields
        Src.Record fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> infixInExpr name foundRegions fieldExpr)
                foundRegions
                fields
        Src.Unit -> foundRegions
        Src.Tuple exprA exprB exprs ->
            List.foldl (infixInExpr name)
                foundRegions
                (exprA : exprB : exprs)
        Src.Shader _ _ -> foundRegions


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
findQualNameInType qual name tipe =
  case tipe of
    Src.TLambda arg ret ->
      findQualNameInType qual name (A.toValue arg) ++ findQualNameInType qual name (A.toValue ret)

    Src.TVar name ->
      []

    Src.TType region _ tlist ->
      concatMap (findQualNameInType qual name . A.toValue) tlist

    Src.TTypeQual region qual_ name_ tlist ->
      if qual_ == qual && name == name_ then
        region : concatMap (findQualNameInType qual name . A.toValue) tlist
      else
        concatMap (findQualNameInType qual name . A.toValue) tlist

    Src.TRecord fields extRecord ->
      concatMap (\a -> findQualNameInType qual name (A.toValue (snd a))) fields

    Src.TUnit ->
      []

    Src.TTuple a b rest ->
      findQualNameInType qual name (A.toValue a)
      ++ findQualNameInType qual name (A.toValue b)
      ++ concatMap (findQualNameInType qual name . A.toValue) rest


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
findNameInType name tipe =
  case tipe of
    Src.TLambda arg ret ->
      findNameInType name (A.toValue arg) ++ findNameInType name (A.toValue ret)

    Src.TVar name ->
      []

    Src.TType region name_ tlist ->
      if name == name_ then
        region : concatMap (findNameInType name . A.toValue) tlist
      else
        concatMap (findNameInType name . A.toValue) tlist

    Src.TTypeQual _ _ _ tlist ->
      concatMap (findNameInType name . A.toValue) tlist

    Src.TRecord fields extRecord ->
      concatMap (\a -> findNameInType name (A.toValue (snd a))) fields

    Src.TUnit ->
      []

    Src.TTuple a b rest ->
      findNameInType name (A.toValue a)
      ++ findNameInType name (A.toValue b)
      ++ concatMap (findNameInType name . A.toValue) rest


findTypeInExpr :: (Src.Type_ -> [A.Region]) -> Src.Expr -> [A.Region]
findTypeInExpr f expr =
  findTypeInExprHelp f [] expr


findTypeInExprHelp :: (Src.Type_ -> [A.Region]) -> [A.Region] -> Src.Expr -> [A.Region]
findTypeInExprHelp f found (A.At region expr_) =
    case expr_ of
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


diagnostics :: FilePath -> [FilePath] -> IO (Either DiagnosticsExit [(FilePath, Int, [Reporting.Report.Report])])
diagnostics filePath remain =
  do  maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot

      case maybeRoot of
        Nothing ->
          return $ Left DiagnosticsExitNoRoot

        Just root ->
          do  files <- findElmFilesInSourceDirs root
                         & fmap (filter (\a -> a /= filePath))

              result <-
                Dir.withCurrentDirectory root $
                  BW.withScope $ \scope -> Stuff.withRootLock root $
                    Task.run $
                      do  details <- Task.eio DiagnosticsExitBadDetails $
                                       Details.load Reporting.silent scope root

                          artifacts <- Task.eio DiagnosticsExitBadBuild $
                                         Build.fromPaths Reporting.silent
                                           root
                                           details
                                           (Data.NonEmptyList.List filePath files)

                          return (artifacts, details)


              case result of
                Right (artifacts, details) -> do

                  fmap Right $ mapM
                    (\path -> do
                      let projectType =
                            case Details._outline details of
                              Details.ValidApp _ -> Parse.Application
                              Details.ValidPkg name _ _ -> (Parse.Package name)

                      warnings <-  warnings projectType root path

                      case warnings of
                        Nothing ->
                          return $ ( path, 2, [] )

                        Just ( sourceMod, warns ) ->
                          do  let warningToReport :: Reporting.Warning.Warning -> Reporting.Report.Report
                                  warningToReport =
                                    Reporting.Warning.toReportForLs
                                      (Reporting.Render.Type.Localizer.fromModule sourceMod)
                              return $ ( path, 2, map warningToReport warns )
                    )
                    -- FIXME: Get warnings only for one file, they are quite slow.
                    (filePath : [])

                Left (DiagnosticsExitBadBuild buildProblem) -> do
                  case Reporting.Exit.toBuildProblemReport buildProblem of
                    (Reporting.Exit.Help.CompilerReport filePath e es) ->
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





-- LOAD SINGLE


data SingleFileResult = Single
  { _source :: Either Reporting.Error.Syntax.Error Src.Module,
    _warnings :: Maybe [Reporting.Warning.Warning],
    _interfaces :: Maybe (Map.Map ModuleName.Raw Interface.Interface),
    _canonical :: Maybe Can.Module,
    _compiled :: Maybe (Either Reporting.Error.Error Compile.Artifacts)
  }



data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw Interface.Interface
    , _graph :: Opt.GlobalGraph
    }




-- WARNINGS

warnings :: Parse.ProjectType -> FilePath -> FilePath -> IO (Maybe (Src.Module, [ Reporting.Warning.Warning ]))
warnings projectType root path =
  do  loaded <- loadSingle projectType root path

      let (Single source maybeWarnings interfaces canonical compiled) =
            addAliasOptionsToWarnings $
              addUnusedDeclarations $
              addUnusedImports loaded

      return $ case source of
        Right sourceMod -> Just (sourceMod, Maybe.fromMaybe [] maybeWarnings)
        Left _ -> Nothing



{-
The below function also modifies the canonical AST by hydrating missing types.

-}
-- @TODO this is a disk mode function
loadSingle :: Parse.ProjectType -> FilePath -> FilePath -> IO SingleFileResult
loadSingle projectType root path =
  Dir.withCurrentDirectory root $
    do  source <- File.readUtf8 path
        case Parse.fromByteString projectType source of
          Right srcModule ->
            do  ifacesResult <- allInterfaces root (Data.NonEmptyList.List path [])
                (Artifacts packageIfaces globalGraph) <- allPackageArtifacts root
                pure $ case ifacesResult of
                  Left exit ->
                    -- report exit : Exit.Reactor?
                      Single
                        (Right srcModule)
                        Nothing
                        Nothing
                        Nothing
                        Nothing

                  Right localIfaces ->
                    let ifaces = Map.union localIfaces packageIfaces
                        (canWarnings, eitherCanned) =
                          Reporting.Result.run $
                            Canonicalize.Module.canonicalize
                              (case projectType of
                                  Parse.Application -> Pkg.dummyName
                                  Parse.Package name -> name
                              )
                              ifaces
                              srcModule
                    in
                    case eitherCanned of
                      Left errs ->
                          Single
                            (Right srcModule)
                            (Just canWarnings)
                            (Just ifaces)
                            Nothing
                            (Just (Left (Reporting.Error.BadNames errs)))

                      Right initialCanModule ->
                        let canModule = addMissingTypes canWarnings initialCanModule
                        in
                        case typeCheck srcModule canModule of
                          Left typeErrors ->
                            Single
                              (Right srcModule)
                              (Just canWarnings)
                              (Just ifaces)
                              (Just canModule)
                              ( Just
                                  ( Left
                                      ( Reporting.Error.BadTypes
                                          (Reporting.Render.Type.Localizer.fromModule srcModule)
                                          typeErrors
                                      )
                                  )
                              )

                          Right annotations ->
                            let nitpicks = Nitpick.PatternMatches.check canModule

                                (optWarnings, eitherLocalGraph) =
                                  Reporting.Result.run $
                                    Optimize.Module.optimize annotations canModule
                            in
                            case eitherLocalGraph of
                              Left errs ->
                                Single
                                  (Right srcModule)
                                  (Just (canWarnings <> optWarnings))
                                  (Just ifaces)
                                  (Just canModule)
                                  ( Just
                                      ( Left
                                          ( Reporting.Error.BadMains
                                              (Reporting.Render.Type.Localizer.fromModule srcModule)
                                              errs
                                          )
                                      )
                                  )

                              Right localGraph ->
                                Single
                                  (Right srcModule)
                                  (Just (canWarnings <> optWarnings))
                                  (Just ifaces)
                                  (Just canModule)
                                  ( Just
                                      ( case nitpicks of
                                          Right () ->
                                            Right (Compile.Artifacts canModule annotations localGraph)

                                          Left errors ->
                                            Left (Reporting.Error.BadPatterns errors)
                                      )
                                  )

          Left err ->
            pure
              ( Single
                  (Left err)
                  Nothing
                  Nothing
                  Nothing
                  Nothing
              )


allInterfaces :: FilePath -> Data.NonEmptyList.List FilePath -> IO (Either Reporting.Exit.Reactor (Map.Map ModuleName.Raw Interface.Interface))
allInterfaces root paths =
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $
        do  details <- Task.eio Reporting.Exit.ReactorBadDetails $
              Details.load Reporting.silent scope root
            artifacts <- Task.eio Reporting.Exit.ReactorBadBuild $
              Build.fromPaths Reporting.silent root details paths

            Task.io $ extractInterfaces root $ Build._modules artifacts


extractInterfaces :: FilePath -> [Build.Module] -> IO (Map.Map ModuleName.Raw Interface.Interface)
extractInterfaces root modu =
  do  k <-
        mapM
          ( \m ->
              case m of
                Build.Fresh nameRaw ifaces _ ->
                  pure $ Just (nameRaw, ifaces)
                Build.Cached name _ mCachedInterface ->
                  cachedHelp root name mCachedInterface
          )
          modu

      pure $ Map.fromList $ Maybe.catMaybes k


{- Appropriated from Build.loadInterface -}
cachedHelp :: FilePath -> ModuleName.Raw -> Control.Concurrent.MVar.MVar Build.CachedInterface -> IO (Maybe (ModuleName.Raw, Interface.Interface))
cachedHelp root name ciMvar = do
  cachedInterface <- Control.Concurrent.MVar.takeMVar ciMvar
  case cachedInterface of
    Build.Corrupted ->
      do  Control.Concurrent.MVar.putMVar ciMvar cachedInterface
          return Nothing

    Build.Loaded iface ->
      do  Control.Concurrent.MVar.putMVar ciMvar cachedInterface
          return (Just (name, iface))

    Build.Unneeded ->
      do  maybeIface <- File.readBinary (Stuff.elmi root name)
          case maybeIface of
            Nothing ->
              do  Control.Concurrent.MVar.putMVar ciMvar Build.Corrupted
                  return Nothing

            Just iface ->
              do  Control.Concurrent.MVar.putMVar ciMvar (Build.Loaded iface)
                  return (Just (name, iface))


{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allPackageArtifacts :: FilePath ->  IO Artifacts
allPackageArtifacts root =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- Control.Concurrent.MVar.readMVar imvar
              mobjs <- Control.Concurrent.MVar.readMVar omvar
              case Control.Monad.liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ root

                Just (deps, objs) ->
                  return $ Artifacts (toInterfaces deps) objs


toInterfaces :: Map.Map ModuleName.Canonical Interface.DependencyInterface -> Map.Map ModuleName.Raw Interface.Interface
toInterfaces deps =
  Map.mapMaybe toUnique $ Map.fromListWith OneOrMore.more $
    Map.elems (Map.mapMaybeWithKey getPublic deps)


toUnique :: OneOrMore.OneOrMore a -> Maybe a
toUnique oneOrMore =
  case oneOrMore of
    OneOrMore.One value -> Just value
    OneOrMore.More _ _  -> Nothing


getPublic :: ModuleName.Canonical -> Interface.DependencyInterface -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore Interface.Interface)
getPublic (ModuleName.Canonical _ name) dep =
  case dep of
    Interface.Public  iface -> Just (name, OneOrMore.one iface)
    Interface.Private _ _ _ -> Nothing


addMissingTypes :: [Reporting.Warning.Warning] -> Can.Module -> Can.Module
addMissingTypes warnings canModul =
  let lookup :: Map.Map Name.Name Can.Type
      lookup =
        Map.fromList $
          Maybe.catMaybes $
            fmap
              (\warning ->
                case warning of
                  Reporting.Warning.MissingTypeAnnotation region name annotation ->
                    Just (name, annotation)

                  _ ->
                    Nothing
              )
              warnings
   in
   canModul {Can._decls = addMissingTypeToDecl lookup (Can._decls canModul)}


addMissingTypeToDecl :: Map.Map Name.Name Can.Type -> Can.Decls -> Can.Decls
addMissingTypeToDecl lookup decl =
  case decl of
    Can.Declare def moarDecls ->
      Can.Declare
        (addMissingTypeToDef lookup def)
        (addMissingTypeToDecl lookup moarDecls)

    Can.DeclareRec def defs moarDecls ->
      Can.DeclareRec
        (addMissingTypeToDef lookup def)
        (fmap (addMissingTypeToDef lookup) defs)
        (addMissingTypeToDecl lookup moarDecls)

    Can.SaveTheEnvironment ->
      Can.SaveTheEnvironment


addMissingTypeToDef :: Map.Map Name.Name Can.Type -> Can.Def -> Can.Def
addMissingTypeToDef typeLookup def =
  case def of
    Can.Def locatedName patterns expr ->
      let maybeType = Map.lookup (A.toValue locatedName) typeLookup
      in
      case maybeType of
        Nothing ->
          def

        Just tipe ->
          Can.TypedDef locatedName Map.empty (fmap (\patt -> (patt, Can.TUnit)) patterns) expr tipe

    _ ->
      def


typeCheck ::
  Src.Module
    -> Can.Module
    -> Either
        (Data.NonEmptyList.List Reporting.Error.Type.Error)
        (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  System.IO.Unsafe.unsafePerformIO (Type.run =<< Type.constrain canonical)


addAliasOptionsToWarnings :: SingleFileResult -> SingleFileResult
addAliasOptionsToWarnings untouched@(Single source maybeWarnings maybeInterfaces canonical compiled) =
    case (canonical, maybeInterfaces, maybeWarnings) of
        (Just canModule, Just interfaces, Just warnings) ->
            let newWarnings = fmap (addAliasesHelper canModule interfaces) warnings
            in
            Single source (Just newWarnings) maybeInterfaces canonical compiled

        _ ->
            untouched


addAliasesHelper :: Can.Module -> Data.Map.Map ModuleName.Raw Interface.Interface -> Reporting.Warning.Warning -> Reporting.Warning.Warning
addAliasesHelper canModule interfaces warning =
    case warning of
        Reporting.Warning.UnusedImport _ _ ->
            warning

        Reporting.Warning.UnusedVariable _ _ _ ->
            warning

        Reporting.Warning.MissingTypeAnnotation region name canType ->
            Reporting.Warning.MissingTypeAnnotation region name (addAliasesToType canModule interfaces canType)


addAliasesToType :: Can.Module -> Data.Map.Map ModuleName.Raw Interface.Interface -> Can.Type -> Can.Type
addAliasesToType canModule interfaces canType =
  case canType of
    Can.TLambda one two ->
      Can.TLambda
        (addAliasesToType canModule interfaces one)
        (addAliasesToType canModule interfaces two)

    Can.TVar _ ->
      canType

    Can.TType moduleName name children ->
      Can.TType moduleName name
        (fmap (addAliasesToType canModule interfaces) children)

    Can.TRecord fieldMap maybeName ->
      case getAliasForRecord fieldMap canModule interfaces of
        Nothing -> canType
        Just aliasFound -> aliasFound

    Can.TUnit ->
      canType

    Can.TTuple one two Nothing ->
      Can.TTuple
        (addAliasesToType canModule interfaces one)
        (addAliasesToType canModule interfaces two)
        Nothing

    Can.TTuple one two (Just three) ->
      Can.TTuple
        (addAliasesToType canModule interfaces one)
        (addAliasesToType canModule interfaces two)
        (Just ((addAliasesToType canModule interfaces three)))

    Can.TAlias moduleName name vars (Can.Holey holeyType) ->
      canType

    Can.TAlias moduleName name vars (Can.Filled holeyType) ->
      canType


getAliasForRecord :: Data.Map.Map Name Can.FieldType -> Can.Module -> Data.Map.Map ModuleName.Raw Interface.Interface -> Maybe Can.Type
getAliasForRecord fields canModule interfaces =
    let (Can.Module _name _exports _docs _decls _unions aliases _binops _effects) = canModule
        matches = Data.Map.foldrWithKey (getMatchingAliases canModule fields) [] aliases
    in
    case matches of
      [] -> Nothing
      (top : _) -> Just top


getMatchingAliases :: Can.Module -> Data.Map.Map Name Can.FieldType -> Name -> Can.Alias -> [Can.Type] -> [Can.Type]
getMatchingAliases canModule fields aliasName (Can.Alias vars aliasType) gathered =
  case aliasType of
    Can.TRecord aliasFieldMap maybeName ->
      -- We only care about matching aliases for records
      -- Everything else can contribte to obfuscation
      case fulfillAliasVars fields vars aliasFieldMap of
        Nothing ->
          gathered

        Just unifiedVars ->
          let (Can.Module moduleName _ _ _ _ _ _ _) = canModule
          in
          Can.TAlias moduleName aliasName unifiedVars (Can.Filled aliasType) : gathered

    Can.TLambda one two ->
     gathered

    Can.TVar _ ->
     gathered

    Can.TType moduleName name children ->
     gathered

    Can.TUnit ->
     gathered

    Can.TTuple one two Nothing ->
     gathered

    Can.TTuple one two (Just three) ->
     gathered

    Can.TAlias moduleName name myAliasVars (Can.Holey holeyType) ->
     gathered

    Can.TAlias moduleName name myAliasVars (Can.Filled holeyType) ->
     gathered


{-|
  Make sure oneFields is a subrecord of twoFields.

  And fill in what the vars should be for the alias.

-}
fulfillAliasVars :: Data.Map.Map Name Can.FieldType -> [Name] -> Data.Map.Map Name Can.FieldType -> Maybe [(Name, Can.Type)]
fulfillAliasVars oneFields aliasVars twoFields =
    let aliasVarMap = Data.Map.fromList $ fmap (\name -> (name, Can.TVar name)) aliasVars

        unificationResult =
          Data.Map.foldrWithKey
            (\key value maybeVars ->
              case maybeVars of
                Nothing ->
                  -- something failed somewhere
                    Nothing
                Just vars ->
                  case Data.Map.lookup key twoFields of
                    Nothing ->
                      Nothing

                    Just twoValue ->
                      case unifyFieldType value twoValue of
                        Nothing ->
                          Nothing

                        Just unifiedVars ->
                          Just (vars ++ unifiedVars)
            )
            (Just [])
            oneFields
    in
    case unificationResult of
      Nothing ->
        Nothing

      Just varsResolved ->
        Just varsResolved


unifyFieldType :: Can.FieldType -> Can.FieldType -> Maybe [(Name, Can.Type)]
unifyFieldType (Can.FieldType _ one) (Can.FieldType _ two) =
  unifyType one two


unifyType :: Can.Type -> Can.Type -> Maybe [(Name, Can.Type)]
unifyType one two =
  case (one, two) of
    (firstType, Can.TVar twoVarName) ->
      Just [(twoVarName, firstType)]

    (Can.TRecord oneFields maybeName, Can.TRecord twoFields twoMaybeName) ->
      let (newVars, finalRemainingFields) =
            Data.Map.foldrWithKey
              (\key value (maybeVars, remainingTwoFields) ->
                case maybeVars of
                  Nothing ->
                    -- something failed somewhere
                    ( Nothing
                    , remainingTwoFields
                    )

                  Just vars ->
                    case Data.Map.lookup key remainingTwoFields of
                      Nothing ->
                        ( Nothing
                        , remainingTwoFields
                        )

                      Just twoValue ->
                        case unifyFieldType value twoValue of
                          Nothing ->
                            ( Nothing
                            , Data.Map.delete key remainingTwoFields
                            )

                          Just unifiedVars ->
                            ( Just (vars ++ unifiedVars)
                            , Data.Map.delete key remainingTwoFields
                            )
              )
              (Just [], twoFields)
              oneFields
      in
      if Data.Map.size finalRemainingFields > 0 then
        Nothing

      else
        newVars

    (Can.TLambda oneOne oneTwo, Can.TLambda twoOne twoTwo) ->
        (++)
          <$> unifyType oneOne twoOne
          <*> unifyType oneTwo twoTwo


    (Can.TType oneModuleName oneName oneVars, Can.TType twoModuleName twoName twoVars) ->
      if oneModuleName == twoModuleName && oneName == twoName then
        -- Wrong
        Just []

      else
        Nothing

    (Can.TUnit, Can.TUnit) ->
      Just []

    (Can.TTuple oneOne oneTwo Nothing, Can.TTuple twoOne twoTwo Nothing) ->
      (++)
        <$> unifyType oneOne twoOne
        <*> unifyType oneTwo twoTwo

    (Can.TTuple oneOne oneTwo (Just oneThree), Can.TTuple twoOne twoTwo (Just twoThree)) ->
      (\a b c ->
        a ++ b ++ b
      )
        <$> unifyType oneOne twoOne
        <*> unifyType oneTwo twoTwo
        <*> unifyType oneThree twoThree

    (Can.TAlias oneModuleName oneName oneVars _, Can.TAlias twoModuleName twoName twoVars _) ->
      if oneModuleName == twoModuleName && oneName == twoName then
        Just oneVars

      else
        Nothing

    _ ->
      Nothing


addUnusedDeclarations :: SingleFileResult -> SingleFileResult
addUnusedDeclarations untouched@(Single source warnings interfaces canonical compiled) =
  case canonical of
    Nothing -> untouched

    Just canModule ->
      let (Can.Module _ exports _ decls _ _ _ _) = canModule
      in
      case exports of
        Can.ExportEverything _ ->
          untouched

        Can.Export exportMap -> do
          let usedValues = usedValues_ canModule
          let (Can.Module _ _ _ decls _ _ _ _) = canModule
          let unusedDecls = filterOutUsedDecls (Can._name canModule) exportMap usedValues decls
          let unusedDeclWarnings = fmap declsToWarning unusedDecls
          Single source (addUnused unusedDeclWarnings warnings) interfaces canonical compiled


usedValues_ :: Can.Module -> Set.Set (ModuleName.Canonical, Name)
usedValues_ (Can.Module name exports docs decls unions aliases binops effects) =
  usedValueInDecls decls Set.empty


declsToWarning :: Can.Def -> Reporting.Warning.Warning
declsToWarning unusedDef =
  case unusedDef of
    Can.Def locatedName pattern expr ->
      Reporting.Warning.UnusedVariable (A.toRegion locatedName) Reporting.Warning.Def (A.toValue locatedName)

    Can.TypedDef locatedName frevars pattern expr type_ ->
      Reporting.Warning.UnusedVariable (A.toRegion locatedName) Reporting.Warning.Def (A.toValue locatedName)


filterOutUsedDecls :: ModuleName.Canonical -> Data.Map.Map Name (A.Located Can.Export) ->  Set.Set (ModuleName.Canonical, Name) -> Can.Decls -> [Can.Def]
filterOutUsedDecls modName exportMap used decls =
  case decls of
    Can.SaveTheEnvironment ->
      []

    Can.Declare def moarDecls ->
      let name = getDefIdentifier def
          identifier = (modName, name)
      in
      if Set.member identifier used || Data.Map.member name exportMap then
        filterOutUsedDecls modName exportMap used moarDecls
      else
        [def] <> filterOutUsedDecls modName exportMap used moarDecls

    Can.DeclareRec def defs moarDecls ->
      let name = getDefIdentifier def
          identifier = (modName, name)
      in
      if Set.member identifier used || Data.Map.member name exportMap then
        filterOutUsedDecls modName exportMap used moarDecls
      else
        [def] <> filterOutUsedDecls modName exportMap used moarDecls


getDefIdentifier :: Can.Def -> Name
getDefIdentifier def =
  case def of
    Can.Def locatedName _ _ ->
      (A.toValue locatedName)

    Can.TypedDef locatedName _ _ _ _ ->
      (A.toValue locatedName)


usedValueInDecls :: Can.Decls -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInDecls decls found =
  case decls of
    Can.Declare def moarDecls ->
      usedValueInDecls moarDecls $ (usedValueInDef def found)

    Can.DeclareRec def defs moarDecls ->
      usedValueInDecls moarDecls $ List.foldl (flip usedValueInDef) found (def : defs)

    Can.SaveTheEnvironment ->
      found


usedValueInDef :: Can.Def -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInDef def found =
  case def of
    Can.Def _ patterns expr ->
      usedValueInExpr expr found

    Can.TypedDef _ _ patternTypes expr tipe ->
      usedValueInExpr expr found


usedValuesInBranch :: Can.CaseBranch -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValuesInBranch (Can.CaseBranch pattern expr) found =
  usedValueInExpr expr found


usedValueInExpr :: Can.Expr -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInExpr (A.At pos expr) found =
  case expr of
    Can.VarLocal _ ->
     found

    Can.VarTopLevel canMod name ->
     Set.insert (canMod, name) found

    Can.VarKernel _ _ ->
     found

    Can.VarForeign canMod name annotation ->
     Set.insert (canMod, name) found

    Can.VarCtor _ canMod name index annotation ->
     Set.insert (canMod, name) found

    Can.VarDebug canMod name annotation ->
     Set.insert (canMod, name) found

    Can.VarOperator _ canMod name annotation ->
     Set.insert (canMod, name) found

    Can.Chr _ ->
     found

    Can.Str _ ->
     found

    Can.Int _ ->
     found

    Can.Float _ ->
     found

    Can.List exprList ->
     List.foldr usedValueInExpr found exprList

    Can.Negate expr ->
     usedValueInExpr expr found

    Can.Binop _ canMod name annotation exprOne exprTwo ->
     usedValueInExpr exprTwo $ usedValueInExpr exprOne $ Set.insert (canMod, name) found

    Can.Lambda patternList expr ->
     usedValueInExpr expr found

    Can.Call expr exprList ->
     usedValueInExpr expr $ List.foldr usedValueInExpr found exprList

    Can.If listTuple expr ->
      usedValueInExpr expr $
        List.foldr
          (\(oneExpr, twoExpr) f ->
            usedValueInExpr twoExpr $ usedValueInExpr oneExpr f
          )
          found listTuple

    Can.Let def expr ->
      usedValueInExpr expr $ usedValueInDef def $ found

    Can.LetRec defList expr ->
      usedValueInExpr expr $ List.foldr usedValueInDef found defList

    Can.LetDestruct pattern oneExpr twoExpr ->
      usedValueInExpr twoExpr $ usedValueInExpr oneExpr $ found

    Can.Case expr branches ->
      List.foldr usedValuesInBranch (usedValueInExpr expr found) $
        branches

    Can.Accessor _ ->
      found

    Can.Access expr _ ->
      usedValueInExpr expr found

    Can.Update _ expr record ->
      usedValueInExpr expr $
        List.foldl
          (\innerFound (Can.FieldUpdate _ expr) -> usedValueInExpr expr innerFound)
          found
          (Map.elems record)

    Can.Record record ->
      List.foldl (flip usedValueInExpr) found $ Map.elems record

    Can.Unit ->
      found

    Can.Tuple one two Nothing ->
      usedValueInExpr two $ usedValueInExpr one found

    Can.Tuple one two (Just three) ->
      usedValueInExpr three $ usedValueInExpr two $ usedValueInExpr one found

    Can.Shader _ _ ->
      found


addUnusedImports :: SingleFileResult -> SingleFileResult
addUnusedImports untouched@(Single source warnings interfaces canonical compiled) =
  case source of
    Left _ ->
      untouched

    Right srcModule ->
      case fmap usedModules canonical of
        Nothing ->
          untouched

        Just usedModules ->
          let (Src.Module _ _ _ imports _ _ _ _ _) = srcModule
              filteredImports = filterOutDefaultImports imports
              importNames = Set.fromList $ fmap Src.getImportName filteredImports
              usedModuleNames = Set.map canModuleName usedModules
              unusedImports = Set.difference importNames usedModuleNames
              unusedImportWarnings = importsToWarnings (Set.toList unusedImports) filteredImports
          in
          Single source (addUnused unusedImportWarnings warnings) interfaces canonical compiled


-- By default every Elm module has these modules imported with these region pairings.
-- If they add a manual import of, e.g. `import Maybe`, then we'll get the same name
-- but with a non-zero based region
filterOutDefaultImports :: [Src.Import] -> [Src.Import]
filterOutDefaultImports imports =
  filter
    (\(Src.Import (A.At region name) _ _) ->
      not $ any (\defaultImport -> defaultImport == (name,region)) defaultImports
    )
    imports


defaultImports :: [(Name, A.Region)]
defaultImports =
  [ ("Platform.Sub", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform.Cmd", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Tuple", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Char", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("String", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Result", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Maybe", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("List", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Debug", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Basics", A.Region (A.Position 0 0) (A.Position 0 0))
  ]


importsToWarnings :: [Name] -> [Src.Import] -> [Reporting.Warning.Warning]
importsToWarnings unusedNames imports =
  importsToWarningsHelper unusedNames imports []


importsToWarningsHelper :: [Name] -> [Src.Import] -> [Reporting.Warning.Warning] -> [Reporting.Warning.Warning]
importsToWarningsHelper unusedNames imports warnings =
  case imports of
    [] -> warnings
    (Src.Import (A.At region name) _ _) : remainingImports ->
      if any (\unusedName -> unusedName == name) unusedNames
        then importsToWarningsHelper unusedNames remainingImports (Reporting.Warning.UnusedImport region name : warnings)
        else importsToWarningsHelper unusedNames remainingImports warnings


canModuleName :: ModuleName.Canonical -> Name
canModuleName (ModuleName.Canonical pkg modName) =
  modName


addUnused :: [Reporting.Warning.Warning] -> Maybe [Reporting.Warning.Warning] -> Maybe [Reporting.Warning.Warning]
addUnused newWarnings maybeExisting =
  case maybeExisting of
    Nothing -> Just newWarnings
    Just old -> Just (old <> newWarnings)


usedModules :: Can.Module -> Set.Set ModuleName.Canonical
usedModules (Can.Module name exports docs decls unions aliases binops effects) =
  Set.unions
    [ usedInDecls decls Set.empty
    , Map.foldr (usedInUnion) Set.empty unions
    , Map.foldr (usedInAlias) Set.empty aliases
    ]


usedInAlias :: Can.Alias -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInAlias (Can.Alias _ tipe) found =
  usedInType tipe found


usedInUnion :: Can.Union -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInUnion (Can.Union _ constructors _ _) found =
  List.foldl
    (\innerFound (Can.Ctor _ _ _ tipes) ->
      List.foldl (\f tipe -> usedInType tipe f) innerFound tipes
    )
    found
    constructors


usedInType :: Can.Type -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInType type_ found =
  case type_ of
    Can.TLambda typeOne typeTwo ->
      usedInType typeTwo $ usedInType typeOne $ found

    Can.TVar _ ->
      found

    Can.TType modName name types ->
      Set.insert modName $ List.foldl (flip usedInType) found types

    Can.TRecord fields _ ->
      List.foldr (\(Can.FieldType _ tipe) -> usedInType tipe)
        found
        fields

    Can.TUnit ->
      found

    Can.TTuple one two Nothing ->
      usedInType two $ usedInType one $ found

    Can.TTuple one two (Just three) ->
      usedInType three $ usedInType two $ usedInType one $ found

    Can.TAlias modName _ fields (Can.Holey aliasType) ->
      Set.insert modName $
        usedInType aliasType $
        List.foldr (\(_, tipe) -> usedInType tipe) found fields

    Can.TAlias modName _ fields (Can.Filled aliasType) ->
      Set.insert modName $
        usedInType aliasType $
        List.foldr (\(_, tipe) -> usedInType tipe) found fields


usedInDecls :: Can.Decls -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInDecls decls found =
  case decls of
    Can.Declare def moarDecls ->
      usedInDecls moarDecls $ (usedInDef def found)

    Can.DeclareRec def defs moarDecls ->
      usedInDecls moarDecls $ List.foldl (flip usedInDef) found (def : defs)

    Can.SaveTheEnvironment ->
      found


usedInDef :: Can.Def -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInDef def found =
  case def of
    Can.Def _ patterns expr ->
      usedInExpr expr $ List.foldl (flip usedInPattern) found patterns

    Can.TypedDef _ _ patternTypes expr tipe ->
      List.foldl
        (\innerFound (pattern, patternTipe) ->
          innerFound
          & usedInPattern pattern
          & usedInType patternTipe
        )
        found
        patternTypes
      & usedInExpr expr
      & usedInType tipe


usedInPattern :: Can.Pattern -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInPattern (A.At _ pattern) found =
  case pattern of
    Can.PCtor modName _ union _ _ args ->
      List.foldl
        (\innerFound (Can.PatternCtorArg _ tipe pattern) ->
          innerFound
          & usedInType tipe
          & usedInPattern pattern
        )
        found
        args
      & usedInUnion union
      & Set.insert modName

    Can.PCons consOne consTwo ->
      usedInPattern consOne found
      & usedInPattern consTwo

    Can.PList patterns ->
      List.foldr usedInPattern found patterns

    Can.PAlias pattern _ ->
      usedInPattern pattern found

    Can.PTuple one two Nothing ->
      usedInPattern one found
      & usedInPattern two

    Can.PTuple one two (Just three) ->
      usedInPattern one found
      & usedInPattern two
      & usedInPattern three

    _ ->
      found


usedInExpr :: Can.Expr -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInExpr (A.At pos expr) found =
  case expr of
    Can.VarLocal _ ->
      found

    Can.VarTopLevel used _ ->
      Set.insert used found

    Can.VarKernel _ _ ->
      found

    Can.VarForeign used _ annotation ->
      Set.insert used found
      & usedInAnnotation annotation

    Can.VarCtor _ used _ index annotation ->
      Set.insert used found
      & usedInAnnotation annotation

    Can.VarDebug used _ annotation ->
      Set.insert used found
      & usedInAnnotation annotation

    Can.VarOperator _ used _ annotation ->
      Set.insert used found
      & usedInAnnotation annotation

    Can.Chr _ ->
      found

    Can.Str _ ->
      found

    Can.Int _ ->
      found

    Can.Float _ ->
      found

    Can.List exprList ->
      List.foldr usedInExpr found exprList

    Can.Negate expr ->
      usedInExpr expr found

    Can.Binop name used _ annotation exprOne exprTwo ->
      Set.insert used found
      & usedInExpr exprOne
      & usedInExpr exprTwo
      & usedInAnnotation annotation

    Can.Lambda patternList expr ->
      List.foldr usedInPattern found patternList
      & usedInExpr expr

    Can.Call expr exprList ->
      List.foldr usedInExpr found exprList
      & usedInExpr expr

    Can.If listTuple expr ->
      List.foldr
        (\(oneExpr, twoExpr) f ->
          usedInExpr oneExpr f
          & usedInExpr twoExpr
        )
        found
        listTuple
      & usedInExpr expr

    Can.Let def expr ->
      found
      & usedInDef def
      & usedInExpr expr

    Can.LetRec defList expr ->
      List.foldr usedInDef found defList
      & usedInExpr expr

    Can.LetDestruct pattern oneExpr twoExpr ->
      found
      & usedInPattern pattern
      & usedInExpr oneExpr
      & usedInExpr twoExpr

    Can.Case expr branches ->
      branches
      & List.foldr usedInBranch (usedInExpr expr found)

    Can.Accessor _ ->
      found

    Can.Access expr _ ->
      usedInExpr expr found

    Can.Update _ expr record ->
      Map.elems record
      & List.foldl (\innerFound (Can.FieldUpdate _ expr) -> usedInExpr expr innerFound) found
      & usedInExpr expr

    Can.Record record ->
      Map.elems record
      & List.foldl (flip usedInExpr) found

    Can.Unit ->
      found

    Can.Tuple one two Nothing ->
      usedInExpr one found
      & usedInExpr two

    Can.Tuple one two (Just three) ->
      usedInExpr one found
      & usedInExpr two
      & usedInExpr three

    Can.Shader _ _ ->
      found


usedInBranch :: Can.CaseBranch -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInBranch (Can.CaseBranch pattern expr) found =
  usedInExpr expr found
  & usedInPattern pattern


usedInAnnotation :: Can.Annotation -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInAnnotation (Can.Forall freevars type_) found =
  usedInType type_ found



-- SYMBOLS


singleFileForSymbols :: State -> FilePath -> IO (Either DefinitionExit SingleFileResult)
singleFileForSymbols state filePath =
  BW.withScope $ \scope ->
  do  maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory filePath) Stuff.findRoot

      case maybeRoot of
        Nothing ->
          return (Left DefinitionExitNoRoot)

        Just root ->
          -- TODO: figure out why root lock stuff is anywhere anyway :D
          do  details <- Stuff.withRootLock root $
                Details.load Reporting.silent scope root

              case details of
                Left exit -> return $ Left $ DefinitionExitBadDetails exit
                Right details_ ->
                  do  let projectType =
                            case Details._outline details_ of
                              Details.ValidApp _ -> Parse.Application
                              Details.ValidPkg pkgName _ _ -> Parse.Package pkgName

                      fmap Right $ loadSingle projectType root filePath


srcModuleSymbols :: Src.Module -> [Aeson.Value]
srcModuleSymbols mod =
  let
    moduleChildren =
      List.concat
        [
          map
            (\(A.At region (Src.Value (A.At nameRegion name) _ _ _)) ->
              SymbolInfo name 12 region nameRegion []
            )
            (Src._values mod),
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
            (Src._unions mod),
          map
            (\(A.At region (Src.Alias (A.At nameRegion name) _ _)) ->
              SymbolInfo name 23 region nameRegion []
            )
            (Src._aliases mod),
          map
            (\(A.At region (Src.Infix name _ _ _)) ->
              SymbolInfo name 25 region region []
            )
            (Src._binops mod)
        ]
        & List.sortOn (\a -> let (A.Region (A.Position start _) _) = _symbol_region a in start)
  in
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
    (Src._name mod)
    & map symbolInfoToJson


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
