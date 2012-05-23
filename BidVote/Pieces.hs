{-# LANGUAGE OverloadedStrings #-}

module BidVote.Pieces where

import Text.Blaze.Html      ( (!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String as S
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..)
                            , getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )

title = H.title "BidVote"
meta = H.meta ! A.charset "utf-8"

setScript = do
	H.script ! A.src "../jQuery/" $ ""

style = do
 	H.style ! A.type_ "text/css" $ cssStyle

content = do
	H.div ! A.id "content"

banner = do
	H.h1 $ do
		H.span ! A.id "o1" $ "Bid"
		H.span ! A.id "g1"  $ "Vote"
		H.span $ fromString $ show (maxBound :: Int)

nav dirs = do
	H.div ! A.id "nav" $ do
		sequence_ $ map navButton dirs

navButton item = do
	H.button ! A.formaction (fromString $ "../" ++ item ++ "/") $ (fromString item)

cssStyle :: Html
cssStyle = do
	" body {  margin: 0; padding: 0; padding-top: 10px; text-align: center; "
	"background-color: #FDFCDC } "
	"#content { width: 800px; text-align: left; "
	"border: 0px; padding: 0; margin: 0 auto; } "
	" h1 {font-size: 170; text-align: center; font-family: sans-serif; "
	"margin-top: 30 px; } "
        "#g1 {color: #458B00 } "
	"#o1 {color: #FF7F24 } "
	"#nav {width: 700px; font-size: 23; margin-top: 90; } "