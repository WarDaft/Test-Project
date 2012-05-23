{-# LANGUAGE OverloadedStrings #-}

module BidVote.Pieces where

import Text.Blaze.Html      ( (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

title = H.title "BidVote"
meta = H.meta ! A.charset "utf-8"

setScript = do
	H.script ! A.src "../jQuery/" $ ""

style = do
 	H.style ! A.type_ "text/css" $ cssStyle

center = do
	H.div ! A.id "center"

cssStyle :: Html
cssStyle = do
	"body { margin: 0; width 100%; padding: 0; padding-top: 0; text-align: center; } "
	"#center { text-align: center; "
	"border: 0px; padding: 0; margin: 0 auto; color: WarmSummer; } "