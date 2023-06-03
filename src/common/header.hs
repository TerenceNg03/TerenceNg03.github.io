{-# LANGUAGE OverloadedStrings #-}

module Header where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

blogHead :: String -> Html
blogHead s = H.head $ do
    H.title $ toHtml s
    link ! rel "stylesheet" ! href "master.css"
