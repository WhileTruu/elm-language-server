{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Reporting
  ( Style
  , silent
  , json
  , terminal
  , languageServer
  --
  , attempt
  , attemptWithStyle
  --
  , Key
  , report
  , ignorer
  , ask
  --
  , DKey
  , DMsg(..)
  , trackDetails
  --
  , BKey
  , BMsg(..)
  , trackBuild
  --
  , reportGenerate
  )
  where


import Control.Concurrent
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import Control.Monad (when)
import qualified Data.ByteString.Builder as B
import qualified Data.NonEmptyList as NE
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

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Aeson ((.:), (.=))


-- STYLE


data Style
  = Silent
  | Json
  | Terminal (MVar ())
  | LanguageServer


silent :: Style
silent =
  Silent


json :: Style
json =
  Json


terminal :: IO Style
terminal =
  Terminal <$> newMVar ()


languageServer :: Style
languageServer =
  LanguageServer


-- ATTEMPT


attempt :: (x -> Help.Report) -> IO (Either x a) -> IO a
attempt toReport work =
  do  result <- work `catch` reportExceptionsNicely
      case result of
        Right a ->
          return a

        Left x ->
          do  Exit.toStderr (toReport x)
              Exit.exitFailure


attemptWithStyle :: Style -> (x -> Help.Report) -> IO (Either x a) -> IO a
attemptWithStyle style toReport work =
  do  result <- work `catch` reportExceptionsNicely
      case result of
        Right a ->
          return a

        Left x ->
          case style of
            Silent ->
              do  Exit.exitFailure

            Json ->
              do  B.hPutBuilder stderr (Encode.encodeUgly (Exit.toJson (toReport x)))
                  Exit.exitFailure

            Terminal mvar ->
              do  readMVar mvar
                  Exit.toStderr (toReport x)
                  Exit.exitFailure



-- MARKS


goodMark :: D.Doc
goodMark =
  D.green $ if isWindows then "+" else "●"


badMark :: D.Doc
badMark =
  D.red $ if isWindows then "X" else "✗"


isWindows :: Bool
isWindows =
  Info.os == "mingw32"



-- KEY


newtype Key msg = Key (msg -> IO ())


report :: Key msg -> msg -> IO ()
report (Key send) msg =
  send msg


ignorer :: Key msg
ignorer =
  Key (\_ -> return ())



-- ASK


ask :: D.Doc -> IO Bool
ask doc =
  do  Help.toStdout doc
      askHelp


askHelp :: IO Bool
askHelp =
  do  hFlush stdout
      input <- getLine
      case input of
        ""  -> return True
        "Y" -> return True
        "y" -> return True
        "n" -> return False
        _   ->
          do  putStr "Must type 'y' for yes or 'n' for no: "
              askHelp


-- DETAILS


type DKey = Key DMsg


trackDetails :: Style -> (DKey -> IO a) -> IO a
trackDetails style callback =
  case style of
    Silent ->
      callback (Key (\_ -> return ()))

    Json ->
      callback (Key (\_ -> return ()))

    Terminal mvar ->
      do  chan <- newChan

          _ <- forkIO $
            do  takeMVar mvar
                detailsLoop chan (DState 0 0 0 0 0 0 0)
                putMVar mvar ()

          answer <- callback (Key (writeChan chan . Just))
          writeChan chan Nothing
          return answer

    LanguageServer ->
      do  chan <- newChan

          _ <- forkIO $
            do  sendCreateWorkDoneProgress "details"
                sendProgressBegin "details" "Verifying dependencies..."
                languageServerDetailsLoop chan (DState 0 0 0 0 0 0 0)

          answer <- callback (Key (writeChan chan . Just))
          writeChan chan Nothing
          return answer


detailsLoop :: Chan (Maybe DMsg) -> DState -> IO ()
detailsLoop chan state@(DState total _ _ _ _ built _) =
  do  msg <- readChan chan
      case msg of
        Just dmsg ->
          detailsLoop chan =<< detailsStep dmsg state

        Nothing ->
          putStrLn $ clear (toBuildProgress total total) $
            if built == total
            then "Dependencies ready!"
            else "Dependency problem!"


data DState =
  DState
    { _total :: !Int
    , _cached :: !Int
    , _requested :: !Int
    , _received :: !Int
    , _failed :: !Int
    , _built :: !Int
    , _broken :: !Int
    }


data DMsg
  = DStart Int
  | DCached
  | DRequested
  | DReceived Pkg.Name V.Version
  | DFailed Pkg.Name V.Version
  | DBuilt
  | DBroken


detailsStep :: DMsg -> DState -> IO DState
detailsStep msg (DState total cached rqst rcvd failed built broken) =
  case msg of
    DStart numDependencies ->
      return (DState numDependencies 0 0 0 0 0 0)

    DCached ->
      putTransition (DState total (cached + 1) rqst rcvd failed built broken)

    DRequested ->
      do  when (rqst == 0) (putStrLn "Starting downloads...\n")
          return (DState total cached (rqst + 1) rcvd failed built broken)

    DReceived pkg vsn ->
      do  putDownload goodMark pkg vsn
          putTransition (DState total cached rqst (rcvd + 1) failed built broken)

    DFailed pkg vsn ->
      do  putDownload badMark pkg vsn
          putTransition (DState total cached rqst rcvd (failed + 1) built broken)

    DBuilt ->
      putBuilt (DState total cached rqst rcvd failed (built + 1) broken)

    DBroken ->
      putBuilt (DState total cached rqst rcvd failed built (broken + 1))


putDownload :: D.Doc -> Pkg.Name -> V.Version -> IO ()
putDownload mark pkg vsn =
  Help.toStdout $ D.indent 2 $
    mark
    <+> D.fromPackage pkg
    <+> D.fromVersion vsn
    <> "\n"


putTransition :: DState -> IO DState
putTransition state@(DState total cached _ rcvd failed built broken) =
  if cached + rcvd + failed < total then
    return state

  else
    do  let char = if rcvd + failed == 0 then '\r' else '\n'
        putStrFlush (char : toBuildProgress (built + broken + failed) total)
        return state


putBuilt :: DState -> IO DState
putBuilt state@(DState total cached _ rcvd failed built broken) =
  do  when (total == cached + rcvd + failed) $
        putStrFlush $ '\r' : toBuildProgress (built + broken + failed) total
      return state


toBuildProgress :: Int -> Int -> [Char]
toBuildProgress built total =
  "Verifying dependencies (" ++ show built ++ "/" ++ show total ++ ")"


clear :: [Char] -> [Char] -> [Char]
clear before after =
  '\r' : replicate (length before) ' ' ++ '\r' : after


languageServerDetailsLoop :: Chan (Maybe DMsg) -> DState -> IO ()
languageServerDetailsLoop chan state@(DState total _ _ _ _ built _) =
  do  msg <- readChan chan
      case msg of
        Just dmsg ->
          languageServerDetailsLoop chan =<< languageServerDetailsStep dmsg state

        Nothing ->
          if built == total
            then sendProgressEnd "details" "Dependencies ready!"
            else sendProgressEnd "details" "Dependency problem!"


languageServerDetailsStep :: DMsg -> DState -> IO DState
languageServerDetailsStep msg (DState total cached rqst rcvd failed built broken) =
  case msg of
    DStart numDependencies ->
      return (DState numDependencies 0 0 0 0 0 0)

    DCached ->
      languageServerPutTransition (DState total (cached + 1) rqst rcvd failed built broken)

    DRequested ->
      do  when (rqst == 0) (sendProgressReport "details" "Starting downloads...")
          return (DState total cached (rqst + 1) rcvd failed built broken)

    DReceived pkg vsn ->
      do  languageServerPutDownload "●" pkg vsn
          languageServerPutTransition (DState total cached rqst (rcvd + 1) failed built broken)

    DFailed pkg vsn ->
      do  languageServerPutDownload "X" pkg vsn
          languageServerPutTransition (DState total cached rqst rcvd (failed + 1) built broken)

    DBuilt ->
      languageServerPutBuilt (DState total cached rqst rcvd failed (built + 1) broken)

    DBroken ->
      languageServerPutBuilt (DState total cached rqst rcvd failed built (broken + 1))


languageServerPutTransition :: DState -> IO DState
languageServerPutTransition state@(DState total cached _ rcvd failed built broken) =
  if cached + rcvd + failed < total then
    return state

  else
    do  sendProgressReport "details" $ toBuildProgress (built + broken + failed) total
        return state


languageServerPutBuilt :: DState -> IO DState
languageServerPutBuilt state@(DState total cached _ rcvd failed built broken) =
  do  when (total == cached + rcvd + failed) $
        sendProgressReport "details" $  toBuildProgress (built + broken + failed) total
      return state


languageServerPutDownload :: String -> Pkg.Name -> V.Version -> IO ()
languageServerPutDownload mark pkg vsn =
  sendProgressReport "details" $
    mark
    ++ " " ++ Pkg.toChars pkg
    ++ V.toChars vsn


-- BUILD


type BKey = Key BMsg

type BResult a = Either Exit.BuildProblem a


trackBuild :: Style -> (BKey -> IO (BResult a)) -> IO (BResult a)
trackBuild style callback =
  case style of
    Silent ->
      callback (Key (\_ -> return ()))

    Json ->
      callback (Key (\_ -> return ()))

    Terminal mvar ->
      do  chan <- newChan

          _ <- forkIO $
            do  takeMVar mvar
                putStrFlush "Compiling ..."
                buildLoop chan 0
                putMVar mvar ()

          result <- callback (Key (writeChan chan . Left))
          writeChan chan (Right result)
          return result

    LanguageServer ->
      do  chan <- newChan

          _ <- forkIO $
            do  sendCreateWorkDoneProgress "build"
                sendProgressBegin "build" "Compiling"
                languageServerBuildLoop chan 0

          result <- callback (Key (writeChan chan . Left))
          writeChan chan (Right result)
          return result


data BMsg
  = BDone


buildLoop :: Chan (Either BMsg (BResult a)) -> Int -> IO ()
buildLoop chan done =
  do  msg <- readChan chan
      case msg of
        Left BDone ->
          do  let !done1 = done + 1
              putStrFlush $ "\rCompiling (" ++ show done1 ++ ")"
              buildLoop chan done1

        Right result ->
          let
            !message = toFinalMessage done result
            !width = 12 + length (show done)
          in
          putStrLn $
            if length message < width
            then '\r' : replicate width ' ' ++ '\r' : message
            else '\r' : message


toFinalMessage :: Int -> BResult a -> [Char]
toFinalMessage done result =
  case result of
    Right _ ->
      case done of
        0 -> "Success!"
        1 -> "Success! Compiled 1 module."
        n -> "Success! Compiled " ++ show n ++ " modules."

    Left problem ->
      case problem of
        Exit.BuildBadModules _ _ [] ->
          "Detected problems in 1 module."

        Exit.BuildBadModules _ _ (_:ps) ->
          "Detected problems in " ++ show (2 + length ps) ++ " modules."

        Exit.BuildProjectProblem _ ->
          "Detected a problem."


languageServerBuildLoop :: Chan (Either BMsg (BResult a)) -> Int -> IO ()
languageServerBuildLoop chan done =
  do  msg <- readChan chan
      case msg of
        Left BDone ->
          do  let !done1 = done + 1
              sendProgressReport "build" $ show done1
              languageServerBuildLoop chan done1

        Right result ->
          let
            !message = toFinalMessage done result
          in
          sendProgressEnd "build" message


-- GENERATE


reportGenerate :: Style -> NE.List ModuleName.Raw -> FilePath -> IO ()
reportGenerate style names output =
  case style of
    Silent ->
      return ()

    Json ->
      return ()

    Terminal mvar ->
      do  readMVar mvar
          let cnames = fmap ModuleName.toChars names
          putStrLn ('\n' : toGenDiagram cnames output)


toGenDiagram :: NE.List [Char] -> FilePath -> [Char]
toGenDiagram (NE.List name names) output =
  let
    width = 3 + foldr (max . length) (length name) names
  in
  case names of
    [] ->
      toGenLine width name ('>' : ' ' : output ++ "\n")

    _:_ ->
      unlines $
        toGenLine width name (vtop : hbar : hbar : '>' : ' ' : output)
        : reverse (zipWith (toGenLine width) (reverse names) ([vbottom] : repeat [vmiddle]))


toGenLine :: Int -> [Char] -> [Char] -> [Char]
toGenLine width name end =
  "    " ++ name ++ ' ' : replicate (width - length name) hbar ++ end


hbar :: Char
hbar = if isWindows then '-' else '─'

vtop :: Char
vtop = if isWindows then '+' else '┬'

vmiddle :: Char
vmiddle = if isWindows then '+' else '┤'

vbottom :: Char
vbottom = if isWindows then '+' else '┘'


--


putStrFlush :: String -> IO ()
putStrFlush str =
  hPutStr stdout str >> hFlush stdout



-- REPORT EXCEPTIONS NICELY


reportExceptionsNicely :: SomeException -> IO a
reportExceptionsNicely e =
  case fromException e of
    Just UserInterrupt -> throw e
    _                  -> putException e >> throw e


putException :: SomeException -> IO ()
putException e = do
  hPutStrLn stderr ""
  Help.toStderr $ D.stack $
    [ D.dullyellow "-- ERROR -----------------------------------------------------------------------"
    , D.reflow $
        "I ran into something that bypassed the normal error reporting process!\
        \ I extracted whatever information I could from the internal error:"
    , D.vcat $ map (\line -> D.red ">" <> "   " <> D.fromChars line) (lines (show e))
    , D.reflow $
        "These errors are usually pretty confusing, so start by asking around on one of\
        \ forums listed at https://elm-lang.org/community to see if anyone can get you\
        \ unstuck quickly."
    , D.dullyellow "-- REQUEST ---------------------------------------------------------------------"
    , D.reflow $
        "If you are feeling up to it, please try to get your code down to the smallest\
        \ version that still triggers this message. Ideally in a single Main.elm and\
        \ elm.json file."
    , D.reflow $
        "From there open a NEW issue at https://github.com/elm/compiler/issues with\
        \ your reduced example pasted in directly. (Not a link to a repo or gist!) Do not\
        \ worry about if someone else saw something similar. More examples is better!"
    , D.reflow $
        "This kind of error is usually tied up in larger architectural choices that are\
        \ hard to change, so even when we have a couple good examples, it can take some\
        \ time to resolve in a solid way."
    ]


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
      [ "jsonrpc" .= ("2.0" :: String)
      , "method" .= method
      , "params" .= value
      ]
   in do
   BSC.hPutStr stdout (BSC.pack header `BSC.append` content)
   hFlush stdout

