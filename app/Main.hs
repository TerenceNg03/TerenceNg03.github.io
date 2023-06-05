{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Lib (blogIndex)
import Photos (blogPhotos)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.Process (readProcess)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5

writeHtml :: (Html, String) -> IO ()
writeHtml (htmlContent, path) = do
    putStrLn $ "Writing " ++ filePath
    createDirectoryIfMissing True $ takeDirectory filePath
    writeFile filePath (renderHtml htmlContent)
  where
    filePath = "./build/" ++ path

rsync :: String -> IO ()
rsync dir = do
    msg <- readProcess "rsync" ["-avR", dir, "build/"] ""
    putStrLn $ "Sync " ++ dir ++ " files..."
    putStrLn msg

main :: IO ()
main = do
    rsync "css/"
    rsync "data/"
    writeHtml blogIndex
    photos <- blogPhotos
    mapM_ writeHtml photos
