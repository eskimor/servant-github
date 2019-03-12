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
-- |
-- Module      : Network.GitHub.Client
-- Copyright   : (c) Finlay Thompson, 2015
-- License     : BSD3
-- Maintainer  : finlay.thompson@gmail.com
-- Stability   : experimental

module Network.GitHub.Client
    ( github
    , AuthToken
    , GitHub
    , runGitHubClientM
    , runGitHubNotApiClientM
    , runGitHub'
    , runGitHub
    , GitHubState(..)
    , HasGitHub
    , embedGitHub
    , EmbedGitHub
    , AddHeaders
    , ReadHeaders
    , Single
    , Paginated
    , setUserAgent
    , resetPagination
    , recurseOff
    , recurseOn
    , pageSize
    , getLinks
    )
where

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
import Servant.Client

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Network.HTTP.Link.Types
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import qualified Network.HTTP.Types.Header as HTTP (Header)

import Network.GitHub.Types (CountedList(..))

import Network.Github.Client.Core

-- | The 'GitHub' monad provides execution context
type GitHub = GitHubCore ClientM

#if !MIN_VERSION_servant_client(0,13,0)
mkClientEnv = ClientEnv
#endif

runGitHubClientM :: ClientM a -> IO (Either ServantError a)
runGitHubClientM comp = do
    manager <- newManager tlsManagerSettings
    runClientM comp (mkClientEnv manager host)

-- | Most of the time we must use api.github.com, but calling
-- login/oauth/access_token only works if sent to github.com.
runGitHubNotApiClientM :: ClientM a -> IO (Either ServantError a)
runGitHubNotApiClientM comp = do
    manager <- newManager tlsManagerSettings
    runClientM comp (mkClientEnv manager hostNotApi)

-- | You need to provide a 'Maybe AuthToken' to lift a 'GitHub' computation
-- into the 'IO' monad.
runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp token = runGitHubClientM $ runGitHubCore comp token

