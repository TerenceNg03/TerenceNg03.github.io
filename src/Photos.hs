{-# LANGUAGE OverloadedStrings #-}

module Photos (blogPhotos) where

import Common (blogFooter, blogHeader, htmlHead)
import Control.Monad (filterM, forM_)
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
photosIndex allN currN = H.div ! class_ "page-index" $
    forM_ [1 .. allN] $ \idx ->
        a
            ! href (fromString $ "photo" ++ show idx ++ ".html")
            !? (idx == currN, class_ "page-index-current")
            $ fromString
            $ "[" ++ show idx ++ "]"

photoPage :: Int -> Int -> [String] -> Html
photoPage allN currN photos = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Photos" $
        link ! rel "stylesheet" ! (href . baseL' $ "css/photos.css")
    body $ do
        blogHeader baseL (Just "photo")
        H.div ! class_ "image" $
            img ! src (dataBaseL "cover.jpg") ! alt "Cover"
        H.div ! class_ "intro" $ p "終需消散的 · 但試過捉緊"
        forM_ photos $ \photo ->
            H.div ! class_ "display" $ img ! src (dataBaseL $ "display/" ++ photo) ! alt "photo"
        photosIndex allN currN
        blogFooter

groupN :: Int -> [a] -> [[a]]
groupN n l =
    if null l
        then []
        else take n l : groupN n (drop n l)

blogPhotos :: IO [(Html, String)]
blogPhotos = do
    let readPath = "data/photos/display/"
    dirContents <- getDirectoryContents readPath
    files <- filterM doesFileExist $ (readPath ++) <$> dirContents
    let files' = groupN 2 $ takeFileName <$> files
        allN = length files'
    putStrLn "Photos group :"
    print files'
    return $ flip (<$>) (zip [1 ..] files') $ \(idx, file) ->
        (photoPage allN idx file, "photos/photo" ++ show idx ++ ".html")
