{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Stream
       (
       -- * StreamingAPI
         userstream
       , statusesFilter
       , statusesSample
  ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types
import Web.Twitter.Conduit.Utils

import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Network.HTTP.Types as HT
import Control.Monad.IO.Class

($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
rsrc $=+ cndt = do
  (src, finalizer) <- C.unwrapResumable rsrc
  return $ CI.ResumableSource (src C.$= cndt) finalizer

userstream :: TwitterBaseM m => HT.SimpleQuery -> TW WithToken m (C.ResumableSource (TW WithToken m) StreamingAPI)
userstream query = do
  rsrc <- api authRequired "GET" "https://userstream.twitter.com/1.1/user.json" query
  rsrc $=+ conduitFromJSON

statusesFilter :: TwitterBaseM m => HT.SimpleQuery -> TW WithToken m (C.ResumableSource (TW WithToken m) StreamingAPI)
statusesFilter query = do
  rsrc <- api authRequired "POST" "https://stream.twitter.com/1.1/statuses/filter.json" query
  rsrc $=+ conduitFromJSON

statusesSample :: TwitterBaseM m => HT.SimpleQuery -> TW WithToken m (C.ResumableSource (TW WithToken m) StreamingAPI)
statusesSample query = do
  rsrc <- api authRequired "GET" "https://stream.twitter.com/1.1/statuses/sample.json" query
  rsrc $=+ conduitFromJSON
