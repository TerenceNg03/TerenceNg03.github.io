{-# LANGUAGE OverloadedStrings #-}

module Photos (photoPages) where

import Common (blogFooter, blogHeader, htmlHead)
import Control.Monad (filterM, forM_)
import Data.List (isSuffixOf, sort)
import Data.String (IsString, fromString)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeFileName)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

baseL :: String
baseL = "../"

baseL' :: (IsString a) => String -> a
baseL' s = fromString $ baseL ++ s

dataBaseL :: (IsString a) => String -> a
dataBaseL s = baseL' $ "data/photos/" ++ s

photosIndex :: Int -> Int -> Html
photosIndex allN currN = H.div ! class_ "page-index" $ do
    a ! href "index.html" !? (currN == 1, class_ "page-index-current") $ "[1]"
    forM_ [2 .. allN] $ \idx ->
        a
            ! href (fromString $ "photo" ++ show (idx - 1) ++ ".html")
            !? (idx == currN, class_ "page-index-current")
            $ fromString
            $ "[" ++ show idx ++ "]"

photoHead :: Html -> Html
photoHead s = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Photos" $
        link ! rel "stylesheet" ! (href . baseL' $ "css/photos.css")
    body $ blogHeader baseL (Just "Photo")
    s

photoPage :: Int -> Int -> [String] -> Html
photoPage allN currN photos = docTypeHtml . photoHead $ do
    forM_ photos $ \photo ->
        H.div ! class_ "display" $ img ! src (dataBaseL $ "display/" ++ photo) ! alt "photo"
    photosIndex allN (currN + 1)
    blogFooter

photoCoverPage :: Int -> Html
photoCoverPage allN = photoHead $ do
    H.div ! class_ "image" $
        img ! src (dataBaseL "cover.jpg") ! alt "Cover"
    H.div ! class_ "intro" $ p "終需消散的 · 但試過捉緊"
    photosIndex allN 1
    blogFooter

groupN :: Int -> [a] -> [[a]]
groupN n l =
    if null l
        then []
        else take n l : groupN n (drop n l)

photoPages :: IO [(Html, String)]
photoPages = do
    let readPath = "data/photos/display/"
    dirContents <- getDirectoryContents readPath
    files <-
        filterM doesFileExist $
            (readPath ++) <$> filter (".jpg" `isSuffixOf`) dirContents
    let files' = groupN 3 . sort $ takeFileName <$> files
        allN = length files' + 1
    putStrLn "Photos group :"
    print files'
    return $ ((photoCoverPage allN, "photos/index.html") :) $ flip (<$>) (zip [1 ..] files') $ \(idx, file) ->
        (photoPage allN idx file, "photos/photo" ++ show idx ++ ".html")
