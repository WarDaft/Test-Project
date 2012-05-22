module Templates where

import SimpleHtml1

banner = text "banner"
navigation = text "navigation"
bottomInfo = text "bottomInfo" 

template page =
    html $ do
        body $ do
            table $ do
                tr $ do
                    td $ do
                        banner
                        navigation
                tr $ do
                    td $ do
                        page
                tr $ do
                    td $ do
                        bottomInfo