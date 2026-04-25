{-# OPTIONS_GHC -Wall #-}
module LanguageServer.Infer
  ( inferHoverInfo
  , lookupHoverInfo
  )
  where


import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified System.IO.Unsafe
import Data.Word (Word16, Word32)

import qualified AST.Canonical as Can
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
import qualified LanguageServer.Infer.Type as InferType
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



-- LOOKUP


lookupHoverInfo :: A.Position -> Src.Module -> Map.Map A.Region HoverInfo -> Maybe (A.Region, HoverInfo)
lookupHoverInfo position src regionTypes =
  lookupRecordFieldHover position src regionTypes
    <|> lookupPositionType position regionTypes


lookupPositionType :: A.Position -> Map.Map A.Region HoverInfo -> Maybe (A.Region, HoverInfo)
lookupPositionType position regionTypes =
  case filter (\(region, _) -> isInRegion position region) (Map.toList regionTypes) of
    [] -> Nothing
    matches -> Just (List.minimumBy compareRegionSpan matches)


lookupRecordFieldHover
  :: A.Position
  -> Src.Module
  -> Map.Map A.Region HoverInfo
  -> Maybe (A.Region, HoverInfo)
lookupRecordFieldHover position src regionTypes =
  do  (fieldRegion, fieldName, recordRegion) <- findRecordFieldAtPosition position src
      recordHoverInfo <- Map.lookup recordRegion regionTypes
      fieldType <- getRecordFieldType fieldName (InferType._hoverType recordHoverInfo)
      return
        ( fieldRegion
        , InferType.HoverInfo
            { InferType._hoverName = Just fieldName
            , InferType._hoverKind = InferType.HoverRecordField
            , InferType._hoverType = fieldType
            }
        )


findRecordFieldAtPosition :: A.Position -> Src.Module -> Maybe (A.Region, Name.Name, A.Region)
findRecordFieldAtPosition position src =
  foldr (\value acc -> findRecordFieldInValue position value <|> acc) Nothing (Src._values src)


findRecordFieldInValue :: A.Position -> A.Located Src.Value -> Maybe (A.Region, Name.Name, A.Region)
findRecordFieldInValue position (A.At _ (Src.Value _ _ body _)) =
  findRecordFieldInExpr position body


findRecordFieldInExpr :: A.Position -> Src.Expr -> Maybe (A.Region, Name.Name, A.Region)
findRecordFieldInExpr position (A.At region expr_) =
  case expr_ of
    Src.Record fields ->
      foldr
        (\(field, value) acc ->
          if isInRegion position (A.toRegion field) then
            Just (A.toRegion field, A.toValue field, region)
          else
            findRecordFieldInExpr position value <|> acc
        )
        Nothing
        fields

    Src.Update starter fields ->
      foldr
        (\(field, value) acc ->
          if isInRegion position (A.toRegion field) then
            Just (A.toRegion field, A.toValue field, region)
          else
            findRecordFieldInExpr position value <|> acc
        )
        (if isInRegion position (A.toRegion starter) then Nothing else Nothing)
        fields

    Src.List exprs ->
      foldr (\subExpr acc -> findRecordFieldInExpr position subExpr <|> acc) Nothing exprs

    Src.Negate subExpr ->
      findRecordFieldInExpr position subExpr

    Src.Binops ops final ->
      foldr (\(subExpr, _) acc -> findRecordFieldInExpr position subExpr <|> acc) (findRecordFieldInExpr position final) ops

    Src.Lambda _ body ->
      findRecordFieldInExpr position body

    Src.Call func args ->
      findRecordFieldInExpr position func <|> foldr (\subExpr acc -> findRecordFieldInExpr position subExpr <|> acc) Nothing args

    Src.If branches finally_ ->
      foldr (\(cond, branch) acc -> findRecordFieldInExpr position cond <|> findRecordFieldInExpr position branch <|> acc) (findRecordFieldInExpr position finally_) branches

    Src.Let defs body ->
      findRecordFieldInExpr position body <|> foldr (\def acc -> findRecordFieldInDef position def <|> acc) Nothing defs

    Src.Case subject branches ->
      findRecordFieldInExpr position subject <|> foldr (\(_, branch) acc -> findRecordFieldInExpr position branch <|> acc) Nothing branches

    Src.Accessor _ ->
      Nothing

    Src.Access subExpr _ ->
      findRecordFieldInExpr position subExpr

    Src.Unit ->
      Nothing

    Src.Tuple a b cs ->
      findRecordFieldInExpr position a
        <|> findRecordFieldInExpr position b
        <|> foldr (\subExpr acc -> findRecordFieldInExpr position subExpr <|> acc) Nothing cs

    Src.Chr _ ->
      Nothing

    Src.Str _ ->
      Nothing

    Src.Int _ ->
      Nothing

    Src.Float _ ->
      Nothing

    Src.Var _ _ ->
      Nothing

    Src.VarQual _ _ _ ->
      Nothing

    Src.Op _ ->
      Nothing

    Src.Shader _ _ ->
      Nothing


findRecordFieldInDef :: A.Position -> A.Located Src.Def -> Maybe (A.Region, Name.Name, A.Region)
findRecordFieldInDef position (A.At _ def) =
  case def of
    Src.Define _ _ expr _ ->
      findRecordFieldInExpr position expr

    Src.Destruct _ expr ->
      findRecordFieldInExpr position expr


getRecordFieldType :: Name.Name -> Can.Type -> Maybe Can.Type
getRecordFieldType fieldName tipe =
  case tipe of
    Can.TRecord fields _ ->
      case Map.lookup fieldName fields of
        Just (Can.FieldType _ fieldType) ->
          Just fieldType

        Nothing ->
          Nothing

    Can.TAlias _ _ _ (Can.Filled realType) ->
      getRecordFieldType fieldName realType

    _ ->
      Nothing


isInRegion :: A.Position -> A.Region -> Bool
isInRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
    && (row == endRow && col <= endCol || row < endRow)


compareRegionSpan :: (A.Region, a) -> (A.Region, b) -> Ordering
compareRegionSpan (region1, _) (region2, _) =
  compare (regionSpan region1) (regionSpan region2)


regionSpan :: A.Region -> (Word32, Word16, Word32, Word16)
regionSpan (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  ( endRow - startRow
  , endCol - startCol
  , startRow
  , startCol
  )
