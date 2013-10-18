{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Fetch
       (
       -- * Direct Messages
         directMessages
       -- , directMessagesSent
       -- , directMessagesShowId

       -- * Friends & Followers
       , friendsIds
       , followersIds
       -- , friendshipsExists
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       -- , friendshipsShow
       -- , friendshipsLookup
       -- , friendshipsNoRetweetIds

       -- * Users
       -- , usersLookup
       -- , usersProfileImageScreenName
       -- , usersSearch
       , usersShow
       -- , usersContributees
       -- , usersContributors

       -- * Suggested Users
       -- , usersSuggestions
       -- , usersSuggestionsSlug
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       -- , favorites

       -- * Lists
       , listsAll
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Param
import Web.Twitter.Conduit.Api

import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C

directMessages :: TwitterBaseM m
               => HT.SimpleQuery -- ^ query
               -> C.Source (TW WithToken m) DirectMessage
directMessages = apiWithPages authRequired "direct_messages.json"

friendsIds, followersIds
  :: TwitterBaseM m => UserParam -> C.Source (TW cred m) UserId
friendsIds   q = apiCursor authSupported "friends/ids.json"   (mkUserParam q) "ids"
followersIds q = apiCursor authSupported "followers/ids.json" (mkUserParam q) "ids"

usersShow :: TwitterBaseM m => UserParam -> (TW cred m) User
usersShow q = apiGet authSupported "users/show.json" (mkUserParam q)

listsAll :: TwitterBaseM m => UserParam -> C.Source (TW cred m) List
listsAll q = apiCursor authSupported "lists/all.json" (mkUserParam q) ""

listsMembers :: TwitterBaseM m => ListParam -> C.Source (TW cred m) User
listsMembers q = apiCursor authSupported "lists/members.json" (mkListParam q) "users"

