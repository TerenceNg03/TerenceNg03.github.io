{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Blogs (blogsPages)
import Control.Monad (forM_)
import Index (blogIndex)
import Photos (photoPages)
import Poems (poemsPages)
import Proses (prosesPages)
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
    forM_
        [photoPages, poemsPages, prosesPages, blogsPages]
        (>>= mapM_ writeHtml)
