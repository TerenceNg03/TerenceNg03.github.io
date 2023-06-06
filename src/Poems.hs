{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Poems (poemsPages) where

import Common (blogFooter, blogHeader, htmlHead)
import Control.Monad (filterM, forM_)
import Data.List (isSuffixOf, sort)
import Data.String (IsString (fromString))
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeFileName)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Utils (breakLines)

baseL :: String
baseL = "../"

baseL' :: (IsString a) => String -> a
baseL' s = fromString $ baseL ++ s

dataBaseL :: (IsString a) => String -> a
dataBaseL s = baseL' $ "data/poems/" ++ s

txtToHtml :: String -> (String, Html)
txtToHtml s =
    let title' = takeWhile (/= '\n') s
        removeLine = drop 1 . dropWhile (/= '\n')
        body' = removeLine . removeLine $ s
     in (title',) $ do
            h1 ! class_ "poem-title" $ fromString title'
            p ! class_ "poem-content" $ breakLines body'

poemsHead :: Html -> Html
poemsHead s = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Poems" $ do
        link ! rel "stylesheet" ! (href . baseL' $ "css/poems.css")
    body $ do
        blogHeader baseL (Just "Poems")
        s
        blogFooter

poemsIndex :: [String] -> Html
poemsIndex titles = poemsHead $ do
    H.div ! class_ "image" $
        img ! src (dataBaseL "cover.jpg")
    H.div ! class_ "intro" $
        p "三言二語 · 一世花果 · 半段因緣 · 未可睇化"
    H.div ! class_ "poems" $
        H.div ! class_ "poem-list" $
            forM_ (zip [1 :: Int ..] titles) $ \(idx, title') ->
                p $ do
                    H.span ! class_ "num" $ fromString . printf "%02d" $ idx
                    a ! href (fromString $ show idx ++ ".html") $ toHtml title'

poemPage :: String -> Html
poemPage = poemsHead . snd . txtToHtml

poemsPages :: IO [(Html, String)]
poemsPages = do
    let readPath = "data/poems/"
    dirContents <- getDirectoryContents readPath
    files <-
        filterM doesFileExist $
            (readPath ++) <$> filter (".txt" `isSuffixOf`) dirContents
    txts <- mapM readFile $ sort files
    putStrLn "Poems mapping:"
    print $ zip [1 :: Int ..] (takeFileName <$> sort files)
    return $
        ((poemsIndex (fst . txtToHtml <$> txts), "poems/index.html") :) $
            zip (poemPage <$> txts) $
                (\x -> "poems/" ++ show x ++ ".html") <$> [1 :: Int ..]
