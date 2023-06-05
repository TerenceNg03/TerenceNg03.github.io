{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Index (blogIndex) where

import Common (blogFooter, blogHeader, htmlHead)
import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Utils (breakLines)

baseL :: String
baseL = "./"

baseL' :: (IsString a) => String -> a
baseL' s = fromString $ baseL ++ s

dataBaseL :: (IsString a) => String -> a
dataBaseL s = baseL' $ "data/index/" ++ s

blogIndex :: (Html, String)
blogIndex = (,"index.html") $ docTypeHtml $ do
    htmlHead baseL "Terence Ng - Home" $
        link ! rel "stylesheet" ! (href . baseL' $ "css/index.css")
    body $ do
        blogHeader baseL Nothing
        titleDiv
        poemBody
        blogFooter

titleDiv :: Html
titleDiv = H.div ! class_ "intro-title" $ do
    h1 ! class_ "title" $ "關於我的一些"
    h1 ! class_ "des" $ "About me"

poemBody :: Html
poemBody = do
    H.div ! class_ "image" ! A.style "margin-bottom: 5em;" $ do
        img ! class_ "left-clip" ! src (dataBaseL "Guitar.jpg") ! alt "Guitar"
        p . breakLines $ "走過一些路\n見過一些人\n經過一些季度\n兜轉 幾番 因果"
    H.div ! class_ "image" $ do
        p . breakLines $ "陽光下 睇過一些生死\n萬暗中 望過一些深淵\n靈魂內\n留低最後信仰\n無可退讓"
        img ! class_ "right-clip" ! src (dataBaseL "Chairs.jpg") ! alt "Chairs"
