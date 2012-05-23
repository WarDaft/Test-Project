{-# LANGUAGE OverloadedStrings #-}

module BidVote.Pieces where

import Text.Blaze.Html      ( (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

title = H.title "BidVote"
meta = H.meta ! A.charset "utf-8"

setScript = do
	H.script ! A.src "http://www.defaultstring.com/jQuery/" $ ""

style = do
	H.style ! A.type_ "text/css" $ cssStyle

cssStyle :: Html
cssStyle = do
	"body { margin: 0; padding: 0; padding-top: 30px; text-align: center; } "
	"#centered { width: 700; text-align: left;"
	" border: 0px; padding: 0; margin: 0 auto; color: WarmSummer; }"