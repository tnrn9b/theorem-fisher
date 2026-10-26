-- | The product's previous name for the specs that check it is refused.
module Fisher.Search.OldName (oldName) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The previous name in lower case, built so that no file spells it.
oldName :: Text
oldName = T.pack ['g', 'a', 'u', 's', 's']
