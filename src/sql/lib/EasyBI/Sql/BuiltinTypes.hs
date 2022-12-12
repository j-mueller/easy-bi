{-| Types of SQL operators and constants
-}
module EasyBI.Sql.BuiltinTypes(
  defaultTypeEnv
) where

import qualified Data.Map.Strict               as Map
import           EasyBI.Sql.Effects.Types      (SqlType (..), SqlVar (..),
                                                TyScheme (..), TypeEnv (..))
import           Language.SQL.SimpleSQL.Syntax (Name (..))

{-| Types for builtin operators and constants
-}
defaultTypeEnv :: TypeEnv
defaultTypeEnv =
  TypeEnv
    $ Map.fromList
        [ binOp "+" STNumber STNumber STNumber
        , binOp "-" STNumber STNumber STNumber
        , binOp "*" STNumber STNumber STNumber
        , binOp ">" STNumber STNumber STBool
        , binOp "<" STNumber STNumber STBool
        , constant "true" STBool
        , constant "false" STBool
        , binOp "or" STBool STBool STBool
        -- "=" is overloaded (not polymorphic) but it should be ok to treat it like
        -- a polymorphic function here
        , let v = STVar 0 in (AnOperator [Name Nothing "="], TyScheme [0] (STArr v (STArr v STBool)))
        ]

binOp :: String -> SqlType v -> SqlType v -> SqlType v -> (SqlVar, (TyScheme v (SqlType v)))
binOp nm a b c = (AnOperator [Name Nothing nm], TyScheme [] (STArr a (STArr b c)))

constant :: String -> SqlType v -> (SqlVar, (TyScheme v (SqlType v)))
constant nm tp = (AnIdentifier [Name Nothing nm], TyScheme [] tp)
