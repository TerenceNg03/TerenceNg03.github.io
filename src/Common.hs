{-# LANGUAGE OverloadedStrings #-}

module Common (htmlHead, blogHeader, blogFooter) where

import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

htmlHead :: String -> String -> Html
htmlHead baseLink titleName = H.head $ do
    H.title $ toHtml titleName
    link ! rel "stylesheet" ! href (fromString $ baseLink ++ "css/common/common.css")
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ ""
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"

blogHeader :: String -> Maybe String -> Html
blogHeader baseLink titleName = do
    blogLogo titleName
    blogNav baseLink

blogLogo :: Maybe String -> Html
blogLogo titleName = H.div ! class_ "head-title" $ do
    maybe H.div (const (! class_ "head-name")) titleName $
        H.div ! class_ "logo" $
            "Terence Ng"
    case titleName of
        Nothing -> return ()
        Just titleName' -> H.div ! class_ "cata" $ do
            H.span ! class_ "head-split" $ "|"
            H.span ! class_ "head-part" $ toHtml titleName'

blogNav :: String -> Html
blogNav baseLink = H.div ! class_ "navbar-container" $ do
    script
        "function autoScrollTo(el) {\
        \    var top = $(\"#\" + el).offset().top;\
        \    $(\"html, body\").animate({ scrollTop: top }, 1000);\
        \    }"
    H.div ! class_ "navbar" $ do
        a ! href (fromString $ baseLink ++ "index.html") $ "Home"
        a ! href (fromString $ baseLink ++ "blog/blog.html") $ "Blogs"
        a ! href (fromString $ baseLink ++ "photo/photo.html") $ "Photos"
        a ! href (fromString $ baseLink ++ "prose/prose.html") $ "Proses"
        a ! href (fromString $ baseLink ++ "poem/poem.html") $ "Poems"
        a ! href "#" ! onclick "return false;" ! onmousedown "autoScrollTo('About');" $ "About"

blogFooter :: Html
blogFooter = H.div ! class_ "footer-container" $
    H.div ! class_ "footer" $ do
        H.div ! class_ "quote" $ p "永遠太遠 · 無謂太早 · 分對或錯"
        H.div ! class_ "footer-inner" ! A.id "About" $ do
            H.div ! class_ "copyright" $ "Original Photos & Contents"
            H.div ! class_ "copyright" $ "Copyright ©2021-2023 TerenceNg  All Rights Reserved."
        H.div ! class_ "contact-me" $ do
            a ! href "https://github.com/TerenceNg03" $ "Github Profile"
            "|"
            a ! href "stoicism03@outlook.com" $ "Contact Me"
