{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
    TypeFamilies, RecordWildCards, OverloadedStrings,
    TypeSynonymInstances, NoMonomorphismRestriction #-}

module BidVote (bidVote) where

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
import qualified BidVote.Pieces as P
import qualified Data.ByteString.Lazy.Char8 as C
import Data.String as S

bidVote =  [ (("Home" :: String), home)
           ]


home = do
    H.html $ do
      H.head $ do
        P.title
        P.meta
        P.style
      H.body $ do
        P.setScript
        P.content $ do
          P.banner
          P.nav $ map (S.fromString . fst) bidVote








