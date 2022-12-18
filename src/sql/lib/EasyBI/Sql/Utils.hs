module EasyBI.Sql.Utils(
  renderString,
  renderText
) where

import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as Text
import           Prettyprinter             (Pretty (pretty),
                                            defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Text as Render

renderString :: Pretty a => a -> String
renderString = Text.unpack . renderText

renderText :: Pretty a => a -> Text
renderText = Render.renderLazy . layoutPretty defaultLayoutOptions . pretty
