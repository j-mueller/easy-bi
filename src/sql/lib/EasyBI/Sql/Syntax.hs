{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
{-|
-}
module EasyBI.Sql.Syntax(
  InPredValueF(..),
  ScalarExprF(..)
) where

import           Data.Data                     (Data, Typeable)
import           Data.Functor.Foldable         (Base, Recursive (..))
import           GHC.Generics                  (Generic)
import           Language.SQL.SimpleSQL.Syntax (Comment, CompPredQuantifier,
                                                Frame, IntervalTypeField, Name,
                                                OdbcLiteralType, QueryExpr,
                                                ScalarExpr, SetOperatorName,
                                                SetQuantifier, Sign, SortSpec,
                                                SubQueryExprType, TypeName)
import qualified Language.SQL.SimpleSQL.Syntax as S

-- based on https://github.com/JakeWheat/simple-sql-parser/blob/master/Language/SQL/SimpleSQL/Syntax.lhs

type instance Base ScalarExpr = ScalarExprF ()

inPredValue :: S.InPredValue -> InPredValueF () ScalarExpr
inPredValue = \case
  S.InList a      -> InList () a
  S.InQueryExpr a -> InQueryExpr () a

data InPredValueF a b =
  InList a [b]
  | InQueryExpr a QueryExpr
  deriving stock (Eq, Show, Functor, Data, Typeable, Generic)

{-| Base functor for the 'ScalarExpr' type
-}
data ScalarExprF a b =
    NumLit a String
    | StringLit a String String String
    | IntervalLit
       { ilAnnot   :: a
       , ilSign    :: Maybe Sign -- ^ if + or - used
       , ilLiteral :: String -- ^ literal text
       , ilFrom    :: IntervalTypeField
       , ilTo      :: Maybe IntervalTypeField
       }
    | TypedLit a TypeName String
    | Iden a [Name]
    | Star a
    | Parameter a
    | PositionalArg a Int
    | HostParameter a String (Maybe String)
    | BinOp a b [Name] b
    | PrefixOp a [Name] b
    | PostfixOp a [Name] b
    | SpecialOp a [Name] [b]
    | App a [Name] [b]
    | AggregateApp
        { aggAnnot    :: a
        , aggName     :: [Name]
        , aggDistinct :: SetQuantifier
        , aggArgs     :: [b]
        , aggOrderBy  :: [SortSpec]
        , aggFilter   :: Maybe b
        }
    | AggregateAppGroup
        { aggAnnot :: a
        , aggName  :: [Name]
        , aggArgs  :: [b]
        , aggGroup :: [SortSpec]
        }
    | WindowApp
        { wnAnnot     :: a
        , wnName      :: [Name]
        , wnArgs      :: [b]
        , wnPartition :: [b]
        , wnOrderBy   :: [SortSpec]
        , wnFrame     :: Maybe Frame
        }
    | SpecialOpK a [Name] (Maybe b) [(String, b)]
    | Cast a b TypeName
    | Convert a TypeName b (Maybe Integer)
    | Case
        { caseAnnot :: a
        , caseTest  :: Maybe b
        , caseWhens :: [([b], b)]
        , caseElse  :: Maybe b
        }
    | Parens a b
    | In a Bool b (InPredValueF a b)
    | SubQueryExpr a SubQueryExprType QueryExpr
    | QuantifiedComparison a b [Name] CompPredQuantifier QueryExpr
    | Match a b Bool QueryExpr
    | Array a b [b]
    | ArrayCtor a QueryExpr
    | Collate a b [Name]
    | MultisetBinOp a b SetOperatorName SetQuantifier b
    | MultisetCtor a [b]
    | MultisetQueryCtor a QueryExpr
    | NextValueFor a [Name]
    | VEComment a [Comment] b
    | OdbcLiteral a OdbcLiteralType String
    | OdbcFunc a b
  deriving stock (Eq, Show, Functor, Data, Typeable, Generic)

instance Recursive ScalarExpr where
  project = \case
    S.NumLit s                     -> NumLit () s
    S.StringLit a b c              -> StringLit () a b c
    S.IntervalLit a b c d          -> IntervalLit () a b c d
    S.TypedLit a b                 -> TypedLit () a b
    S.Iden a                       -> Iden () a
    S.Star                         -> Star ()
    S.Parameter                    -> Parameter ()
    S.PositionalArg a              -> PositionalArg () a
    S.HostParameter a b            -> HostParameter () a b
    S.BinOp a b c                  -> BinOp () a b c
    S.PrefixOp a b                 -> PrefixOp () a b
    S.PostfixOp a b                -> PostfixOp () a b
    S.SpecialOp a b                -> SpecialOp () a b
    S.App a b                      -> App () a b
    S.AggregateApp a b c d e       -> AggregateApp () a b c d e
    S.AggregateAppGroup a b c      -> AggregateAppGroup () a b c
    S.WindowApp a b c d e          -> WindowApp () a b c d e
    S.SpecialOpK a b c             -> SpecialOpK () a b c
    S.Cast a b                     -> Cast () a b
    S.Convert a b c                -> Convert () a b c
    S.Case a b c                   -> Case () a b c
    S.Parens a                     -> Parens () a
    S.In a b c                     -> In () a b (inPredValue c)
    S.SubQueryExpr a b             -> SubQueryExpr () a b
    S.QuantifiedComparison a b c d -> QuantifiedComparison () a b c d
    S.Match a b c                  -> Match () a b c
    S.Array a b                    -> Array () a b
    S.ArrayCtor a                  -> ArrayCtor () a
    S.Collate a b                  -> Collate () a b
    S.MultisetBinOp a b c d        -> MultisetBinOp () a b c d
    S.MultisetCtor a               -> MultisetCtor () a
    S.MultisetQueryCtor a          -> MultisetQueryCtor () a
    S.NextValueFor a               -> NextValueFor () a
    S.VEComment a b                -> VEComment () a b
    S.OdbcLiteral a b              -> OdbcLiteral () a b
    S.OdbcFunc a                   -> OdbcFunc () a
