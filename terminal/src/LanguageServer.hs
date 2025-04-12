{-# LANGUAGE OverloadedStrings #-}

module LanguageServer 
  ( run
  )
  where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Maybe as Maybe
import qualified Data.List as List

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

                              return (id, filePath, row, column)
                        ) =<< Aeson.eitherDecode body

                  case result of
                    Left err -> 
                      do  IO.hPutStr IO.stderr ("Error decoding JSON: " ++ err)
                          loop ()

                    Right (id, filePath, row, column) -> 
                      do  result <- Task.run $ findDefinition filePath row column
                          
                          case result of
                            Right (FoundValue value) ->
                              do  respond id $ encodeRegion filePath (A.toRegion value)
                                  loop ()

                            Left err ->
                              do  IO.hSetBuffering IO.stderr IO.NoBuffering
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

-- DEFINITION

data DefinitionExit
  = DefinitionExitBadDetails Reporting.Exit.Details
  | DefinitionExitBadInput BS.ByteString Reporting.Error.Error
  | DefinitionExitNoRoot
  | DefinitionExitNotFound

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

    DefinitionExitNotFound ->
      Reporting.Exit.Help.report "NO DEFINITION" Nothing
        "I tried to find find things under the cursor but could not."
        []

      

findDefinition :: FilePath -> Int -> Int -> Task.Task DefinitionExit Found
findDefinition filePath line character =
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

              maybe (Task.throw DefinitionExitNotFound) return $
                findThingAtPoint line character srcModule


data Found
  = FoundValue (A.Located Src.Value)


findThingAtPoint :: Int -> Int -> Src.Module -> Maybe Found
findThingAtPoint line character (Src.Module name exports docs imports values unions alias infixes effects) =
  List.foldl 
    (\found located ->
      case located of
        A.At region value ->
          if regionContains (A.Position (fromIntegral line) (fromIntegral character)) region 
            then Just (FoundValue located)
            else found 
    )
    Nothing
    values 


regionContains :: A.Position -> A.Region -> Bool
regionContains (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
        && (row == endRow && col <= endCol || row < endRow)


