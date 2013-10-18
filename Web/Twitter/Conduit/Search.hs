{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Search
       (
       -- * Search
         searchTweets
       , searchTweetsSource
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Api

import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Aeson (FromJSON)

searchGet :: (TwitterBaseM m, FromJSON a)
          => AuthHandler cred m
          -> String -- ^ Resource URL
          -> HT.SimpleQuery -- ^ Query
          -> TW cred m a
searchGet hndl uri = apiGet hndl u
  where u = "search/" ++ uri

searchTweets :: TwitterBaseM m
             => String -- ^ search string
             -> HT.SimpleQuery -- ^ query
             -> TW WithToken m (SearchResult [SearchStatus])
searchTweets q query = searchTweets' query'
  where
    query' = ("q", B8.pack q) : query

searchTweets' :: TwitterBaseM m
              => HT.SimpleQuery -- ^ query
              -> TW WithToken m (SearchResult [SearchStatus])
searchTweets' = searchGet authRequired "tweets.json"

searchTweetsSource :: TwitterBaseM m
                   => String -- ^ search string
                   -> HT.SimpleQuery -- ^ query
                   -> TW WithToken m (SearchResult (C.Source (TW WithToken m) SearchStatus))
searchTweetsSource q query = do
    res <- searchTweets q query
    let body = CL.sourceList (searchResultStatuses res)
               <>
               (C.yield (nexts res) C.$= CL.concatMapM pull C.$= CL.concatMap id)
    return $ res { searchResultStatuses = body }
  where
    nexts = searchMetadataNextResults . searchResultSearchMetadata
    pull (Just nq) = do
      let nquery = HT.parseSimpleQuery nq
      let query' = M.toList $ M.union (M.fromList nquery) (M.fromList query)
      res     <- searchTweets' query'
      remains <- pull $ nexts res
      return $ searchResultStatuses res : remains
    pull Nothing = return []
