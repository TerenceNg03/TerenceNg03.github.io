{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Blogs.Utils (mdToHtml, baseL, baseL', dataBaseL, blogsHead) where

import CMark (commonmarkToHtml, optUnsafe)
import Common (blogFooter, blogHeader, htmlHead)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

blogsHead :: Maybe String -> Html -> Html
blogsHead title' s = docTypeHtml $ do
    htmlHead baseL (fromMaybe "Terence Ng - Blogs" title') $ do
        link ! rel "stylesheet" ! (href . baseL' $ "css/blogs.css")
        link ! rel "stylesheet" ! href (dataBaseL "highlight/styles/default.min.css")
        script ! src (dataBaseL "highlight/highlight.min.js") $ ""
        script ! src (dataBaseL "highlight/languages/haskell.min.js") $ ""
        script ! src (dataBaseL "catalog.js") ! defer "" $ ""
    body $ do
        blogHeader baseL (Just "Blogs")
        s
        blogFooter

mdToHtml :: String -> (String, Text)
mdToHtml raw =
    let title' = dropWhile (== '#') . takeWhile (/= '\n') $ raw
        md = "#" ++ title' ++ dropWhile (/= '\n') raw
     in (title',) $ commonmarkToHtml [optUnsafe] . fromString $ md

baseL :: String
baseL = "../"

baseL' :: (IsString a) => String -> a
baseL' s = fromString $ baseL ++ s

dataBaseL :: (IsString a) => String -> a
dataBaseL s = baseL' $ "data/blogs/" ++ s
