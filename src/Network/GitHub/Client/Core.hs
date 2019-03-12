{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.GitHub.Client.Core where

import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Proxy
import GHC.TypeLits
import Data.String
import Text.Read (readMaybe)
import Data.Text.Encoding (decodeUtf8')
import Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Foldable (forM_)
import Data.ByteString (ByteString)

import Servant.API hiding (Link)
import Servant.Client.Core

import Network.HTTP.Link.Types
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import qualified Network.HTTP.Types.Header as HTTP (Header)

import Network.GitHub.Types (CountedList(..))

-- Stolen from Network.Github.Client:
--
-- | Closed type family that adds standard headers to the incoming
-- servant API type. The extra headers are put after any arguments types.
type family AddHeaders a :: * where
    AddHeaders ((sym :: Symbol) :> last)
        = (sym :: Symbol) :> AddHeaders last
    AddHeaders (first :> last)
        = first :> AddHeaders last
    AddHeaders last
        =  Header "User-Agent" Text
        :> Header "Authorization" AuthToken
        :> ReadHeaders last

-- | Token used to authorize access to the GitHub API.
-- see <https://developer.github.com/v3/oauth/>
newtype AuthToken = AuthToken Text deriving (Eq)
instance IsString AuthToken where
    fromString s = AuthToken (fromString s)
instance ToHttpApiData AuthToken where
    toQueryParam (AuthToken t) = T.concat ["token ",  t]

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443 ""

hostNotApi :: BaseUrl
hostNotApi = BaseUrl Https "github.com" 443 ""

-- | The 'GitHub' monad provides execution context
type GitHubCore clientM = ReaderT (Maybe AuthToken) (StateT GitHubState clientM)

runGitHubCore :: GitHubCore clientM a -> Maybe AuthToken -> clientM a
runGitHubCore comp token = evalStateT (runReaderT comp token) defGitHubState

type HeadersToRead =
    '[ Header "Link" Text
     , Header "X-RateLimit-Limit" Int
     , Header "X-RateLimit-Remaining" Int
     , Header "X-RateLimit-Reset" Int
     ]

-- | Closed type family that adds headers necessary for pagination. In particular,
-- it captures the "Link" header from the response.
type family ReadHeaders a :: * where
    ReadHeaders (Get cts [res])
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Get cts (Headers HeadersToRead [res])
    ReadHeaders (Post cts [res])
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Post cts (Headers HeadersToRead [res])
    ReadHeaders (Delete cts [res])
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Delete cts (Headers HeadersToRead [res])
    ReadHeaders (Put cts [res])
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Put cts (Headers HeadersToRead [res])
    ReadHeaders (Patch cts [res])
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Patch cts (Headers HeadersToRead [res])
    ReadHeaders (Get cts (CountedList name res))
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Get cts (Headers HeadersToRead (CountedList name res))
    ReadHeaders (Post cts (CountedList name res))
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Post cts (Headers HeadersToRead (CountedList name res))
    ReadHeaders (Delete cts (CountedList name res))
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Delete cts (Headers HeadersToRead (CountedList name res))
    ReadHeaders (Put cts (CountedList name res))
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Put cts (Headers HeadersToRead (CountedList name res))
    ReadHeaders (Patch cts (CountedList name res))
        = QueryParam "page" Int :> QueryParam "per_page" Int
          :> Patch cts (Headers HeadersToRead (CountedList name res))
    ReadHeaders (Get cts res)
        = Get cts (Headers HeadersToRead res)
    ReadHeaders (Post cts res)
        = Post cts (Headers HeadersToRead res)
    ReadHeaders (Delete cts res)
        = Delete cts (Headers HeadersToRead res)
    ReadHeaders (Put cts res)
        = Put cts (Headers HeadersToRead res)
    ReadHeaders (Patch cts res)
        = Patch cts (Headers HeadersToRead res)
    ReadHeaders otherwise = otherwise

-- | Client function that returns a single result
type Single clientM a = Maybe Text -> Maybe AuthToken
             -> clientM (Headers HeadersToRead a)

-- | Client function that returns a list of results, and is therefore paginated
type Paginated clientM a = Maybe Text -> Maybe AuthToken
                -> Maybe Int -> Maybe Int
                -> clientM (Headers HeadersToRead [a])

-- | Client function that returns a total count and list of results, and is therefore paginated
type CountedPaginated clientM name a = Maybe Text -> Maybe AuthToken
                             -> Maybe Int -> Maybe Int
                             -> clientM (Headers HeadersToRead (CountedList name a))


-- | Closed type family for recursively defining the GitHub client funciton types
type family EmbedGitHub clientM a :: * where
    EmbedGitHub clientM (Single clientM a)  = GitHubCore clientM a
    EmbedGitHub clientM (Paginated clientM a) = GitHubCore clientM [a]
    EmbedGitHub clientM (CountedPaginated clientM name a) = GitHubCore clientM (CountedList name a)
    EmbedGitHub clientM (a -> b) = a -> EmbedGitHub clientM b

readMaybeBS :: Read a => ByteString -> Maybe a
readMaybeBS bs = either (const Nothing) Just (decodeUtf8' bs) >>= readMaybe . T.unpack

updateRateLimit :: HTTP.Header -> StateT GitHubState clientM ()
updateRateLimit = \case
        ("X-RateLimit-Limit",     bs) -> forM_ (readMaybeBS bs) $ \i -> modify $ \pg -> pg {limit = i}
        ("X-RateLimit-Remaining", bs) -> forM_ (readMaybeBS bs) $ \i -> modify $ \pg -> pg {remaining = i}
        ("X-RateLimit-Reset",     bs) -> forM_ (readMaybeBS bs) $ \i -> modify $ \pg -> pg {reset = i}
        _ -> return ()

-- | This class defines how the client code is actually called.
class HasGitHub a clientM | a -> clientM where
    embedGitHub :: a -> EmbedGitHub clientM a
-- | Instance for the case where we have paginated results
instance HasGitHub (Paginated clientM a) clientM where
    embedGitHub comp = do
        token <- ask
        r <- lift $ gets recurse
        when r resetPagination

        let accumPages acc = do
             ua <- gets useragent
             p  <- gets page
             pp <- gets perPage
             hres <- lift $ comp (Just ua) token (Just p) (Just pp)
             forM_ (getHeaders hres) $ \case
                 ("Link", lks) -> modify $ \pg -> pg {links = parseLinkHeaderBS lks}
                 header -> updateRateLimit header
             let acc' = acc ++ getResponse hres
             rec <- gets recurse
             next <- gets hasNextLink
             if rec && next
                 then do
                     modify $ \pg -> pg {page = p + 1}
                     accumPages acc'
                 else return acc'
        lift $ accumPages []

-- | Instance for the case where we have a total count and paginated results
instance HasGitHub (CountedPaginated clientM name a) clientM where
    embedGitHub comp = do
        token <- ask
        r <- lift $ gets recurse
        when r resetPagination

        let accumPages mbCount acc = do
             ua <- gets useragent
             p  <- gets page
             pp <- gets perPage
             hres <- lift $ comp (Just ua) token (Just p) (Just pp)
             forM_ (getHeaders hres) $ \case
                 ("Link", lks) -> modify $ \pg -> pg {links = parseLinkHeaderBS lks}
                 header -> updateRateLimit header
             let response = getResponse hres
                 count = fromMaybe (totalCount response) mbCount
                 acc' = acc ++ items response
             rec <- gets recurse
             next <- gets hasNextLink
             if rec && next
                 then do
                     modify $ \pg -> pg {page = p + 1}
                     accumPages (Just count) acc'
                 else return (CountedList count acc')
        lift $ accumPages Nothing []

-- | Instance for the case where we have single result
instance HasGitHub (Single clientM a) clientM where
    embedGitHub comp = do
        token <- ask
        lift $ do
            ua <- gets useragent
            hres <- lift $ comp (Just ua) token
            forM_ (getHeaders hres) $ \case
                header -> updateRateLimit header
            return $ getResponse hres

-- This instance is a bit too literal. Should be possible to do it reursively
instance HasGitHub (a -> Single clientM b) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> Single clientM c) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> Single clientM d) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> Single clientM e) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> Single clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)

instance HasGitHub (a -> Paginated clientM b) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> Paginated clientM c) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> Paginated clientM d) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> Paginated clientM e) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> l -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> l -> m -> Paginated clientM f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)

instance HasGitHub (a -> CountedPaginated clientM name b) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> CountedPaginated clientM name c) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> CountedPaginated clientM name d) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> CountedPaginated clientM name e) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> l -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)
instance HasGitHub (a -> b -> c -> d -> e -> g -> h -> i -> k -> l -> m -> CountedPaginated clientM name f) clientM where
    embedGitHub comp arg = embedGitHub (comp arg)

-- | Wrapper around the servant 'client' function, that takes care of the
-- extra headers that required for the 'GitHub' monad.
-- #if MIN_VERSION_servant_client(0,13,0)
github :: (HasClient clientM (AddHeaders api), HasGitHub (Client clientM (AddHeaders api)) clientM)
       => Proxy api -> EmbedGitHub clientM (Client clientM (AddHeaders api))
-- #else
-- github :: (HasClient (AddHeaders api), HasGitHub (Client (AddHeaders api)))
--        => Proxy api -> EmbedGitHub (Client (AddHeaders api))
-- #endif
github px = embedGitHub (clientWithHeaders px)

-- #if MIN_VERSION_servant_client(0,13,0)
clientWithHeaders :: (HasClient clientM (AddHeaders api)) => Proxy api -> Client clientM (AddHeaders api)
-- #else
-- clientWithHeaders :: (HasClient (AddHeaders api)) => Proxy api -> Client (AddHeaders api)
-- #endif
clientWithHeaders (Proxy :: Proxy api) = client (Proxy :: Proxy (AddHeaders api))


-- | GitHubState options that control which headers are provided to the API
-- and stores the 'Link' header result
data GitHubState
    = GitHubState
    { perPage    :: Int   -- ^ The number of records returned per page
    , page       :: Int   -- ^ The page number returned
    , links      :: Maybe [Link] -- ^ Contains the returned 'Link' header, if available.
    , recurse    :: Bool  -- ^ Flag to set the recursive mode on
    , useragent  :: Text -- ^ Text to send as "User-agent"
    , limit      :: Int -- ^ The maximum number of requests you're permitted to make per hour
    , remaining  :: Int -- ^ The number of requests remaining in the current rate limit window.
    , reset      :: Int -- ^ The time at which the current rate limit window resets in UTC epoch seconds.
    }
defGitHubState :: GitHubState
defGitHubState = GitHubState 100 1 Nothing True "servant-github" 0 0 0

-- | Overide default value for User-agent header.
-- Note, GitHub requires that a User-agent header be set.
setUserAgent :: Text -> GitHubCore clientM ()
setUserAgent ua = lift $ modify $ \ghs -> ghs { useragent = ua }

hasNextLink :: GitHubState -> Bool
hasNextLink ghs = maybe False hnl (links ghs)
    where hnl = Prelude.any (\ln -> (Rel, "next") `elem` linkParams ln)

-- | Set next page back to 1, and remove the links
resetPagination :: GitHubCore clientM ()
resetPagination = lift $ modify $ \ghs -> ghs { page = 1, links = Nothing }

-- | Turn automatic recusive behaviour on and off.
--
-- If recursive is on, paginated results will be automatically
-- followed and concated together.
recurseOff, recurseOn :: GitHubCore clientM ()
recurseOff =  lift $ modify $ \ghs -> ghs { recurse = False }
recurseOn  =  lift $ modify $ \ghs -> ghs { recurse = True }

-- | The default number of records per page is set to 100. Smaller pages can be
-- set, but not bigger than 100.
pageSize :: Int -> GitHubCore clientM ()
pageSize ps = lift $ modify $ \ghs -> ghs { perPage = ps }

-- | Return the 'Link' header. This is only set when there are futher pages.
getLinks :: GitHubCore clientM (Maybe [Link])
getLinks = lift $ gets links
