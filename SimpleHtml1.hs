{-# LANGUAGE OverloadedStrings #-}

module SimpleHtml1
	( doHtml
	, html, body, table, tr, td, text
	) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary
import Data.Binary.Put

data Html = Html [Html] | String C.ByteString deriving Show

newtype HtmlM a = HtmlM {getHtml :: Html -> (Html, a)}


instance Monad HtmlM where
	return s = HtmlM $ \x -> (x, s)
	x >>= f = HtmlM $ \y -> let
		(d,v) = getHtml x y
		in getHtml (f v) d


htmlPut h = HtmlM $ \x -> (htmlAppend x h, ())
htmlGet = HtmlM $ \x -> (Html [],x)

(+++) = C.append

htmlAppend h1 h2@(String _) = Html [h1,h2]
htmlAppend h1@(String _) (Html h2) = Html $ h1 : h2
htmlAppend h1 h2 = Html $ [h1, h2]

putAll = C.foldl (\x y -> x >> put y) (return ())

open a = String $ runPut $ put '<' >> putAll a >> put '>'
open_ a = String $ runPut $ put '<' >> a >> put '>'
close a = String $ runPut $ put '<' >> put '/' >> putAll a >> put '>'
close_ a = String $ runPut $ put '<' >> put '/' >> a >> put '>'

noviceWrap tag con = con >> htmlGet >>= \contents -> 
     htmlPut
         (Html [open $ tag
               , contents
               , close tag])

-- this is hardly ideal, but none of this is ideal
journeymanWrap tag attrib con = con >> htmlGet >>= \contents -> 
     htmlPut
         (Html [open_ $ putAll tag >> put ' ' >> putAttrib attrib
               , contents
               , close tag])

putAttrib = putAll

doHtml h = runPut . renderHtml . fst $ getHtml h $ String ""

renderHtml (String s) = putAll s
renderHtml (Html list) = foldr (\x y -> renderHtml x >> y)
                               (return ())
                               list

html     = noviceWrap "html"
body     = noviceWrap "body"
table    = noviceWrap "table"
tr       = noviceWrap "tr"
td       = noviceWrap "td"
text     = htmlPut . String