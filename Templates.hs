{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Text.Blaze.Html      ( (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Lazy.Char8
import Text.Blaze.Renderer.Utf8 (renderMarkup)

banner = H.div "banner"
navigation = H.div "navigation"
bottomInfo = H.div "bottomInfo"

template title page = do
    H.docType
    H.html $ do
        H.head $ do
            H.title title
            H.meta ! A.charset "utf-8"
        H.body $ do
            H.table $ do
                H.tr $ do
                    H.td $ do
                        banner
                        navigation
                H.tr $ do
                    H.td $ do
                        page
                H.tr $ do
                    H.td $ do
                        bottomInfo