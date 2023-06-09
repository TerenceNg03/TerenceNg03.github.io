{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Blogs (blogsPages) where

import CMark (commonmarkToHtml, optUnsafe)
import Common (blogFooter, blogHeader, htmlHead)
import Control.Monad (filterM, forM_)
import Data.List (isSuffixOf, sort)
import Data.String (IsString (fromString))
import Data.Text (Text)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeFileName)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)

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

blogsHead :: Html -> Html
blogsHead s = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Blogs" $ do
        link ! rel "stylesheet" ! (href . baseL' $ "css/blogs.css")
        link ! rel "stylesheet" ! href (dataBaseL "highlight/styles/default.min.css")
        script ! src (dataBaseL "highlight/highlight.min.js") $ ""
        script ! src (dataBaseL "highlight/languages/haskell.min.js") $ ""
        script ! src (dataBaseL "catalog.js") ! defer "" $ ""
    body $ do
        blogHeader baseL (Just "Blogs")
        s
        blogFooter

blogsIndex :: [String] -> Html
blogsIndex titles = blogsHead $ do
    H.div ! class_ "image" $
        img ! src (dataBaseL "cover.jpg")
    H.div ! class_ "intro" $
        p "Coding Insight · Guide · Tutorial"
    H.div ! class_ "blogs" $
        H.div ! class_ "blog-list" $
            forM_ (zip [1 :: Int ..] titles) $ \(idx, title') ->
                p $ do
                    H.span ! class_ "num" $ fromString . printf "%02d" $ idx
                    a ! href (fromString $ show idx ++ ".html") $ toHtml title'

blogPage :: String -> Html
blogPage s =
    blogsHead $
        H.div ! class_ "blog-container" $
            preEscapedToHtml . snd $
                mdToHtml s

blogsPages :: IO [(Html, String)]
blogsPages = do
    let readPath = "data/blogs/"
    dirContents <- getDirectoryContents readPath
    files <-
        filterM doesFileExist $
            (readPath ++) <$> filter (".md" `isSuffixOf`) dirContents
    mds <- mapM readFile $ sort files
    putStrLn "Blogs mapping:"
    print $ zip [1 :: Int ..] (takeFileName <$> sort files)
    return $
        ((blogsIndex (fst . mdToHtml <$> mds), "blogs/index.html") :) $
            zip (blogPage <$> mds) $
                (\x -> "blogs/" ++ show x ++ ".html") <$> [1 :: Int ..]
