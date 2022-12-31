module EasyBI.Sql.Utils
  ( renderString
  , renderText
  ) where

import Data.Text.Lazy            (Text)
import Data.Text.Lazy            qualified as Text
import Prettyprinter             (Pretty (pretty), defaultLayoutOptions,
                                  layoutPretty)
import Prettyprinter.Render.Text qualified as Render

renderString :: Pretty a => a -> String
renderString = Text.unpack . renderText

renderText :: Pretty a => a -> Text
renderText = Render.renderLazy . layoutPretty defaultLayoutOptions . pretty
