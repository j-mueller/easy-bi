{-| Types of SQL operators and constants
-}
module EasyBI.Sql.BuiltinTypes(
  defaultTypeEnv
) where

import qualified Data.Map.Strict               as Map
import           EasyBI.Sql.Effects.Types      (SqlType (..), SqlVar (..),
                                                Tp (..), TyScheme (..),
                                                TypeEnv (..))
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
        , binOp "and" STBool STBool STBool
        -- "=" is overloaded (not polymorphic) but it should be ok to treat it like
        -- a polymorphic function here
        , let v = TpVar 0 in (AnOperator [Name Nothing "="], TyScheme [0] (TpArr v (TpArr v (TpSql STBool))))
        ]

binOp :: String -> SqlType -> SqlType -> SqlType -> (SqlVar, (TyScheme v (Tp v)))
binOp nm a b c = (AnOperator [Name Nothing nm], TyScheme [] (TpArr (TpSql a) (TpArr (TpSql b) (TpSql c))))

constant :: String -> SqlType -> (SqlVar, (TyScheme v (Tp v)))
constant nm tp = (AnIdentifier [Name Nothing nm], TyScheme [] (TpSql tp))
