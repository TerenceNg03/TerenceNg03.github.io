{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Proses (prosesPages) where

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

baseL :: String
baseL = "../"

baseL' :: (IsString a) => String -> a
baseL' s = fromString $ baseL ++ s

dataBaseL :: (IsString a) => String -> a
dataBaseL s = baseL' $ "data/proses/" ++ s

mdToHtml :: String -> (String, Text)
mdToHtml raw =
    let title' = dropWhile (== '#') . takeWhile (/= '\n') $ raw
        md = "#" ++ title' ++ dropWhile (/= '\n') raw
     in (title',) $ commonmarkToHtml [optUnsafe] . fromString $ md

prosesHead :: Html -> Html
prosesHead s = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Proses" $ do
        link ! rel "stylesheet" ! (href . baseL' $ "css/proses.css")
    body $ do
        blogHeader baseL (Just "Proses")
        s
        blogFooter

prosesIndex :: [String] -> Html
prosesIndex titles = prosesHead $ do
    H.div ! class_ "image" $
        img ! src (dataBaseL "cover.jpg")
    H.div ! class_ "intro" $
        p "五行十句 · 百種心境 · 三千世界"
    H.div ! class_ "proses" $
        H.div ! class_ "prose-list" $
            forM_ (zip [1 :: Int ..] titles) $ \(idx, title') ->
                p $ do
                    H.span ! class_ "num" $ fromString . printf "%02d" $ idx
                    a ! href (fromString $ show idx ++ ".html") $ toHtml title'

prosePage :: String -> Html
prosePage =
    prosesHead . (H.div ! class_ "prose-container") . preEscapedToHtml . snd . mdToHtml

prosesPages :: IO [(Html, String)]
prosesPages = do
    let readPath = "data/proses/"
    dirContents <- getDirectoryContents readPath
    files <-
        filterM doesFileExist $
            (readPath ++) <$> filter (".md" `isSuffixOf`) dirContents
    mds <- mapM readFile $ sort files
    putStrLn "Proses mapping:"
    print $ zip [1 :: Int ..] (takeFileName <$> sort files)
    return $
        ((prosesIndex (fst . mdToHtml <$> mds), "proses/index.html") :) $
            zip (prosePage <$> mds) $
                (\x -> "proses/" ++ show x ++ ".html") <$> [1 :: Int ..]
