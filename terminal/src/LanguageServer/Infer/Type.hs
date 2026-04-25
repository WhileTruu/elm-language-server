{-# OPTIONS_GHC -Wall #-}
module LanguageServer.Infer.Type
  ( module Base
  , Constraint(..)
  , HoverInfo(..)
  , HoverKind(..)
  , exists
  , toCanType
  )
  where


import qualified AST.Canonical as Can
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import Type.Type as Base hiding (Constraint(..), exists)


data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CSaveTheType A.Region HoverKind (Maybe Name.Name) Base.Type
  | CEqual A.Region E.Category Base.Type (E.Expected Base.Type)
  | CLocal A.Region Name.Name (E.Expected Base.Type)
  | CForeign A.Region Name.Name Can.Annotation (E.Expected Base.Type)
  | CPattern A.Region E.PCategory Base.Type (E.PExpected Base.Type)
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Base.Variable]
      , _flexVars :: [Base.Variable]
      , _header :: Map.Map Name.Name (A.Located Base.Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }


data HoverInfo =
  HoverInfo
    { _hoverName :: Maybe Name.Name
    , _hoverKind :: HoverKind
    , _hoverType :: Can.Type
    }


data HoverKind
  = HoverExpression
  | HoverLocalValue
  | HoverLocalParameter
  | HoverLocalBinding
  | HoverTopLevelValue
  | HoverImportedValue
  | HoverConstructor
  | HoverCustomTypeVariant
  | HoverOperator
  | HoverRecordField
  | HoverFieldAccessor


exists :: [Base.Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue


toCanType :: Base.Variable -> IO Can.Type
toCanType variable =
  do  Can.Forall _ tipe <- Base.toAnnotation variable
      return tipe
