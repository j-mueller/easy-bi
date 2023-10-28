{-| Conveniences for constructing SQL types by hand, for example when testing
the type checker
-}
module EasyBI.Sql.Construct
  ( boolean
  , datetime
  , dot
  , hostParameter
  , int
  , number
  , row
  , temporal
  , text
  ) where

import EasyBI.Sql.Effects.Types      (Tp (..), mkRow)
import EasyBI.Sql.Types              (SqlType (..), SqlVar (..), TyVar (..))
import Language.SQL.SimpleSQL.Syntax (Name (..))

hostParameter :: String -> SqlVar
hostParameter n = AHostParameter n Nothing

dot :: TyVar -> String -> [(String, Tp TyVar)] -> (SqlVar, Tp TyVar)
dot v a values = (AnIdentifier [Name Nothing a], row v values)

row :: TyVar -> [(String, Tp TyVar)] -> Tp TyVar
row v values =
  let f (n, t) = (Name Nothing n, t)
  in TpRow $ mkRow v (f <$> values)

number :: Tp v
number = TpSql STNumber

text :: Tp v
text = TpSql STText

boolean :: Tp v
boolean = TpSql STBool

int :: Tp v
int = TpSql STInt

datetime :: Tp v
datetime = TpSql STDateTime

temporal :: Tp v
temporal = TpSql STTemporal
