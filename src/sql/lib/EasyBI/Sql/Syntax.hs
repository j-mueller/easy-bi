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

type instance Base ScalarExpr = ScalarExprF

inPredValue :: S.InPredValue -> InPredValueF ScalarExpr
inPredValue = \case
  S.InList a      -> InList a
  S.InQueryExpr a -> InQueryExpr a

data InPredValueF b =
  InList [b]
  | InQueryExpr QueryExpr
  deriving stock (Eq, Show, Functor, Data, Typeable, Generic)

{-| Base functor for the 'ScalarExpr' type
-}
data ScalarExprF b =
    NumLit String
    | StringLit String String String
    | IntervalLit
       { ilSign    :: Maybe Sign -- ^ if + or - used
       , ilLiteral :: String -- ^ literal text
       , ilFrom    :: IntervalTypeField
       , ilTo      :: Maybe IntervalTypeField
       }
    | TypedLit TypeName String
    | Iden [Name]
    | Star
    | Parameter
    | PositionalArg Int
    | HostParameter String (Maybe String)
    | BinOp b [Name] b
    | PrefixOp [Name] b
    | PostfixOp [Name] b
    | SpecialOp [Name] [b]
    | App [Name] [b]
    | AggregateApp
        { aggName     :: [Name]
        , aggDistinct :: SetQuantifier
        , aggArgs     :: [b]
        , aggOrderBy  :: [SortSpec]
        , aggFilter   :: Maybe b
        }
    | AggregateAppGroup
        { aggName  :: [Name]
        , aggArgs  :: [b]
        , aggGroup :: [SortSpec]
        }
    | WindowApp
        { wnName      :: [Name]
        , wnArgs      :: [b]
        , wnPartition :: [b]
        , wnOrderBy   :: [SortSpec]
        , wnFrame     :: Maybe Frame
        }
    | SpecialOpK [Name] (Maybe b) [(String, b)]
    | Cast b TypeName
    | Convert TypeName b (Maybe Integer)
    | Case
        { caseTest  :: Maybe b
        , caseWhens :: [([b], b)]
        , caseElse  :: Maybe b
        }
    | Parens b
    | In Bool b (InPredValueF b)
    | SubQueryExpr SubQueryExprType QueryExpr
    | QuantifiedComparison b [Name] CompPredQuantifier QueryExpr
    | Match b Bool QueryExpr
    | Array b [b]
    | ArrayCtor QueryExpr
    | Collate b [Name]
    | MultisetBinOp b SetOperatorName SetQuantifier b
    | MultisetCtor [b]
    | MultisetQueryCtor QueryExpr
    | NextValueFor [Name]
    | VEComment [Comment] b
    | OdbcLiteral OdbcLiteralType String
    | OdbcFunc b
  deriving stock (Eq, Show, Functor, Data, Typeable, Generic)

instance Recursive ScalarExpr where
  project = \case
    S.NumLit s                     -> NumLit s
    S.StringLit a b c              -> StringLit a b c
    S.IntervalLit a b c d          -> IntervalLit a b c d
    S.TypedLit a b                 -> TypedLit a b
    S.Iden a                       -> Iden a
    S.Star                         -> Star
    S.Parameter                    -> Parameter
    S.PositionalArg a              -> PositionalArg a
    S.HostParameter a b            -> HostParameter a b
    S.BinOp a b c                  -> BinOp a b c
    S.PrefixOp a b                 -> PrefixOp a b
    S.PostfixOp a b                -> PostfixOp a b
    S.SpecialOp a b                -> SpecialOp a b
    S.App a b                      -> App a b
    S.AggregateApp a b c d e       -> AggregateApp a b c d e
    S.AggregateAppGroup a b c      -> AggregateAppGroup a b c
    S.WindowApp a b c d e          -> WindowApp a b c d e
    S.SpecialOpK a b c             -> SpecialOpK a b c
    S.Cast a b                     -> Cast a b
    S.Convert a b c                -> Convert a b c
    S.Case a b c                   -> Case a b c
    S.Parens a                     -> Parens a
    S.In a b c                     -> In a b (inPredValue c)
    S.SubQueryExpr a b             -> SubQueryExpr a b
    S.QuantifiedComparison a b c d -> QuantifiedComparison a b c d
    S.Match a b c                  -> Match a b c
    S.Array a b                    -> Array a b
    S.ArrayCtor a                  -> ArrayCtor a
    S.Collate a b                  -> Collate a b
    S.MultisetBinOp a b c d        -> MultisetBinOp a b c d
    S.MultisetCtor a               -> MultisetCtor a
    S.MultisetQueryCtor a          -> MultisetQueryCtor a
    S.NextValueFor a               -> NextValueFor a
    S.VEComment a b                -> VEComment a b
    S.OdbcLiteral a b              -> OdbcLiteral a b
    S.OdbcFunc a                   -> OdbcFunc a
