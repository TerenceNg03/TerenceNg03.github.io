{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Data.Foldable (forM_)
import Lib
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Common.Header

numbers :: Int -> Html
numbers n = docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    body $ do
        p "A list of natural numbers:"
        ul $ forM_ [1 .. n] (li . toHtml)

writeNewFile :: FilePath -> String -> IO ()
writeNewFile path str = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path str

main :: IO ()
main = writeNewFile "./site/index.html" (renderHtml $ numbers 10)
