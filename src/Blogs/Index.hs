{-# LANGUAGE OverloadedStrings #-}

module Blogs.Index (blogsIndex) where

import Blogs.Meta (BlogMeta (..))
import Blogs.Utils (blogsHead, dataBaseL)
import Control.Monad (forM_)
import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)

linkToOnClick :: String -> Attribute
linkToOnClick l =
    onclick . fromString $
        "location.href = \'" ++ l ++ "\';"

indexTemplate :: String -> String -> [(String, String)] -> Html
indexTemplate prompt link' contents' = blogsHead Nothing $ do
    H.div ! class_ "image" $
        img ! src (dataBaseL "cover.jpg")
    H.div ! class_ "intro" $ do
        p $ fromString prompt
        H.div ! class_ "translate" ! linkToOnClick link' $
            img ! src (dataBaseL "translate.png")
    H.div ! class_ "blogs" $
        H.div ! class_ "blog-list" $
            forM_ (zip [1 :: Int ..] contents') $ \(idx, (title', ref)) ->
                p $ do
                    H.span ! class_ "num" $ fromString . printf "%02d" $ idx
                    a ! href (fromString $ ref ++ ".html") $ toHtml title'

merge :: [a] -> [b] -> [(a, b)]
merge (x : xs) (y : ys) = (x, y) : merge xs ys
merge _ _ = []

blogsIndex :: [BlogMeta] -> (Html, Html)
blogsIndex metas =
    let contents_cn = merge (title_cn <$> metas) ((\s -> ref_id s ++ "_cn") <$> metas)
        contents_en = merge (title_en <$> metas) (ref_id <$> metas)
     in ( indexTemplate "Read English Version ➜" "index.html" contents_cn
        , indexTemplate "閱覧正體中文版 ➜" "index_cn.html" contents_en
        )
