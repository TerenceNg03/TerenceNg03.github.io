{-# LANGUAGE OverloadedStrings #-}

module Common (htmlHead, blogHeader, blogFooter) where

import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

htmlHead :: String -> String -> Html -> Html
htmlHead baseLink titleName other = H.head $ do
    H.title $ toHtml titleName
    meta ! charset "utf-8"
    let hrefFixPath s = href (fromString $ baseLink ++ s)
    link ! rel "stylesheet" ! hrefFixPath "css/common/common.css"
    link ! rel "apple-touch-icon" ! sizes "180x180" ! hrefFixPath "favicon/apple-touch-icon.png"
    link ! rel "icon" ! type_ "image/png" ! sizes "32x32" ! hrefFixPath "favicon/favicon-32x32.png"
    link ! rel "icon" ! type_ "image/png" ! sizes "16x16" ! hrefFixPath "favicon/favicon-16x16.png"
    link ! rel "manifest" ! hrefFixPath "favicon/site.webmanifest"
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ ""
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    other

blogHeader :: String -> Maybe String -> Html
blogHeader baseLink titleName = do
    blogLogo titleName
    blogNav baseLink

blogLogo :: Maybe String -> Html
blogLogo titleName = H.div ! class_ "head-title" $ do
    let logo = H.div "Terence Ng"
    case titleName of
        Nothing -> logo ! class_ "logo"
        Just titleName' -> ((logo ! class_ "logo-ani") >>) $ H.div ! class_ "cata" $ do
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
        a ! href (fromString $ baseLink ++ "blogs/index.html") $ "Blogs"
        a ! href (fromString $ baseLink ++ "photos/index.html") $ "Photos"
        a ! href (fromString $ baseLink ++ "poems/index.html") $ "Poems"
        a ! href (fromString $ baseLink ++ "proses/index.html") $ "Proses"
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
            a ! href "stoicism03@gmail.com" $ "Contact Me"
