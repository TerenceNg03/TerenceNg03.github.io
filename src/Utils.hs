module Utils (breakLines) where

import Data.List (foldl1')
import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H

breakLines :: String -> Html
breakLines s =
    let sep = Prelude.map fromString $ lines s
     in foldl1' (\x y -> x >> br >> y) sep
