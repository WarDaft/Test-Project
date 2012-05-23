{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
    TypeFamilies, RecordWildCards, OverloadedStrings,
    TypeSynonymInstances, NoMonomorphismRestriction #-}


module Main where
import Control.Applicative  ( (<$>), optional )
import Control.Exception    ( bracket )
import Control.Monad        ( msum, mzero )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Monad.Trans  ( liftIO )
import Data.Acid            ( AcidState, Query, Update, makeAcidic
                            , openLocalState )
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..)
                            , getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Text            ( Text )
import Data.Text.Lazy       ( toStrict )
import qualified Data.Text  as Text
import Data.Time            ( UTCTime(..), getCurrentTime )
import Happstack.Server

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import BidVote
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Blaze.Renderer.Utf8 (renderMarkup)

instance ToMessage Html where
	toMessage = renderMarkup

response h = ok $ (toResponse h)
htmlResponse h = ok $ (toResponse h) {rsHeaders = (mkHeaders [("Content-Type", "text/html")])}

main :: IO ()
main = do 
    jQuery <- C.readFile "jquery.txt"
    simpleHTTP nullConf {port = 80} $ 
        msum [ dir "jQuery" $ response jQuery
             , dir "BidVote" $ htmlResponse bidVote
             , mainPage
             ]

defaultPolicy :: BodyPolicy
defaultPolicy = (defaultBodyPolicy "/tmp/" 0 50000 1000)

mainPage :: ServerPart Response
mainPage = ok $ toResponse ("hir" :: C.ByteString)

