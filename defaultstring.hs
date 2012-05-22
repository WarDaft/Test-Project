{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
    TypeFamilies, RecordWildCards, OverloadedStrings #-}


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
import Happstack.Server     ( ServerPart, Method(POST, HEAD, GET), Response
                            , decodeBody, defaultBodyPolicy, dir, lookRead
                            , lookText, method, notFound, nullConf
                            , nullDir, ok, seeOther, simpleHTTP, look
                            , toResponse, path, BodyPolicy(..), nullConf
                            , methodM)

import SimpleHtml1
import Dir.BidVote
import qualified Data.ByteString.Lazy.Char8 as C

response h = ok $ toResponse $ doHtml h

main :: IO ()
main = simpleHTTP nullConf $ handlers

defaultPolicy :: BodyPolicy
defaultPolicy = (defaultBodyPolicy "/tmp/" 0 50000 1000)

handlers :: ServerPart Response
handlers =
    do decodeBody defaultPolicy
       msum [ dir "bidvote" $ response $ text "bidvote"
            , mainPage
            ]

mainPage :: ServerPart Response
mainPage = ok $ toResponse ("hi" :: C.ByteString)

