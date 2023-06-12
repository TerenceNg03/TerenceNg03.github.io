{-# LANGUAGE OverloadedStrings #-}

module Blogs.Pages (blogsPage) where

import Blogs.Utils (blogsHead, mdToHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Applicative (Applicative(liftA2))

blogsPage :: String -> IO (Html,Html)
blogsPage ref = 
    let en = readFile $ "data/blogs/" ++ ref ++ "/en.md"
        cn = readFile $ "data/blogs/" ++ ref ++ "/cn.md"
    in liftA2 (,) (page <$> en) (page <$> cn)

page :: String -> Html
page s =
    let title' = Just . dropWhile (=='#') . takeWhile (/= '\n') $ s
    in blogsHead title' $
        H.div ! class_ "blog-container" $
            preEscapedToHtml . snd $
                mdToHtml s
