module Blogs (blogsPages) where

import Blogs.Index (blogsIndex)
import Blogs.Pages (blogsPage)

import Blogs.Meta (BlogMeta (ref_id))
import Control.Applicative (Applicative (liftA2))
import Data.Aeson (decodeFileStrict)
import Data.Maybe (fromJust)
import Text.Blaze.Html5 as H (Html)

blogsPages :: IO [(Html, String)]
blogsPages = do
    let readPath = "data/blogs/"
    meta_data <- decodeFileStrict $ readPath ++ "meta.json" :: IO (Maybe [BlogMeta])
    let meta_data' = fromJust meta_data
        (index_cn, index_en) = blogsIndex meta_data'
        indexes = [(index_cn, "blogs/index_cn.html"), (index_en, "blogs/index.html")]
        refs = ref_id <$> meta_data'
        pages = (concat <$>) $ sequence $ flip fmap refs $ \ref -> do
            (en, cn) <- blogsPage ref
            return [(en, "blogs/" ++ ref ++ ".html"), (cn, "blogs/" ++ ref ++ "_cn.html")]
    liftA2 (++) pages $ return indexes
