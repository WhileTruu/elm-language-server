{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module LanguageServer.Reporting
  ( Key
  , report
  , ignorer
  --
  , RefsKey
  , RefsMsg(..)
  , trackReferences
  , trackDefinition
  , trackDocumentSymbol
  )
  where


import Control.Concurrent
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import Control.Monad (when)
import qualified Data.ByteString.Builder as B
import qualified Data.NonEmptyList as NE
import qualified Data.Time
import qualified Data.Map.Strict as Map
import qualified System.Exit as Exit
import qualified System.Info as Info
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Encode as Encode
import Reporting.Doc ((<+>), (<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Annotation as A

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Aeson ((.:), (.=))



-- KEY


newtype Key msg = Key (msg -> IO ())


report :: Key msg -> msg -> IO ()
report (Key send) msg =
  send msg


ignorer :: Key msg
ignorer =
  Key (\_ -> return ())



-- REFERENCES


type RefsKey = Key RefsMsg


trackReferences :: (RefsKey -> IO a) -> IO a
trackReferences callback =
  do  timeStart <- Data.Time.getCurrentTime
      mvar <- newMVar ()
      chan <- newChan

      _ <- forkIO $
        do  takeMVar mvar
            sendCreateWorkDoneProgress "references"
            sendProgressBegin "references" "🔍 Looking for references"
            referencesLoop timeStart chan 0
            putMVar mvar ()

      answer <- callback (Key (writeChan chan . Left))
      writeChan chan $ Right answer

      return answer


referencesLoop :: Data.Time.UTCTime -> Chan (Either RefsMsg a) -> Int -> IO ()
referencesLoop timeStart chan done =
  do  msg <- readChan chan
      case msg of
        Left (RefsDone amount) ->
          do  let !done1 = done + amount

              sendProgressReport "references" $ "Found " ++ show done1
              referencesLoop timeStart chan done1

        Right _ ->
          do  timeEnd <- Data.Time.getCurrentTime
              let timeDiff = Data.Time.diffUTCTime timeEnd timeStart
              sendProgressEnd "references" $ "Found " ++ show done ++ " (" ++ show timeDiff ++ ")"


data RefsMsg
  = RefsDone Int



-- DEFINITION


trackDefinition :: IO a -> IO a
trackDefinition callback =
  do  timeStart <- Data.Time.getCurrentTime

      sendCreateWorkDoneProgress "definition"
      sendProgressBegin "definition" "👀 Looking for definition"

      answer <- callback

      timeEnd <- Data.Time.getCurrentTime
      let timeDiff = Data.Time.diffUTCTime timeEnd timeStart
      sendProgressEnd "definition" $ "Found (" ++ show timeDiff ++ ")"

      return answer


-- DOCUMENT SYMBOL


trackDocumentSymbol :: IO a -> IO a
trackDocumentSymbol callback =
  do  timeStart <- Data.Time.getCurrentTime

      sendCreateWorkDoneProgress "documentSymbol"
      sendProgressBegin "documentSymbol" "👀 Looking for symbols"

      answer <- callback

      timeEnd <- Data.Time.getCurrentTime
      let timeDiff = Data.Time.diffUTCTime timeEnd timeStart
      sendProgressEnd "documentSymbols" $ "Done (" ++ show timeDiff ++ ")"

      return answer

--


putStrFlush :: String -> IO ()
putStrFlush str =
  hPutStr stdout str >> hFlush stdout



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
   BSC.hPutStr stdout (BSC.pack header `BSC.append` content)
   hFlush stdout

