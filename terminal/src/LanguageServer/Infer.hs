{-# OPTIONS_GHC -Wall #-}
module LanguageServer.Infer
  ( inferHoverInfo
  )
  where


import qualified Data.Map.Strict as Map
import qualified System.IO.Unsafe

import qualified AST.Source as Src
import qualified Canonicalize.Module
import qualified Elm.Interface as Interface
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified LanguageServer.Infer.Module as Infer
import qualified LanguageServer.Infer.Solve as Infer
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Reporting.Result as Result
import LanguageServer.Infer.Type (HoverInfo)


inferHoverInfo
  :: Pkg.Name
  -> Map.Map ModuleName.Raw Interface.Interface
  -> Src.Module
  -> Either Error.Error (Map.Map A.Region HoverInfo)
inferHoverInfo pkg ifaces modul =
  case Result.run (Canonicalize.Module.canonicalize pkg ifaces modul) of
    (_, Left errs) ->
      Left (Error.BadNames errs)

    (_, Right canonical) ->
      case System.IO.Unsafe.unsafePerformIO (Infer.runWithRegions =<< Infer.constrain canonical) of
        Left errs ->
          Left (Error.BadTypes (Localizer.fromModule modul) errs)

        Right (_, regionTypes) ->
          Right regionTypes
