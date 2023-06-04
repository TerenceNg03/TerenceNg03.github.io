{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Lib (blogFooter, blogHeader, htmlHead)
import System.Process (readProcess)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H

index :: Html
index = docTypeHtml $ do
    htmlHead baseL "Terence Ng - Home"
    body $ do
        blogHeader baseL Nothing
        blogFooter
  where
    baseL = "./"

main :: IO ()
main = do
    rsync <- readProcess "rsync" ["-av", "css", "build"] ""
    putStrLn "Sync css files..."
    putStr rsync
    putStrLn "Done sync css files"
    writeFile "./build/index.html" (renderHtml index)
