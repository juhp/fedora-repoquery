{-# LANGUAGE OverloadedStrings #-}

module List (
  listVersionsCmd
  ) where

import Control.Monad
import qualified Data.Text.IO as T
import Network.HTTP.Directory
--import SimpleCmd (error')

import Common
import Query (getFedoraServer)
import Types
import URL

listVersionsCmd :: Verbosity -> RepoSource -> IO ()
-- listVersionsCmd _verbose _reposource =
--   error' "listing Centos Stream versions not supported"
--   --showReleaseCmd mgr server (RepoCentosStream chan) X86_64
-- listVersionsCmd _ RepoKoji =
--   error' "listing Koji versions not supported"
listVersionsCmd verbose reposource = do
  mgr <- httpManager
  -- FIXME handle others
  url <- getFedoraServer mgr reposource ["fedora", "linux", "releases"]
  unless (verbose == Quiet) $ warning $! "<" ++ renderUrl url ++ ">"
  httpDirectory mgr (renderUrl url) >>= mapM_ (T.putStrLn . noTrailingSlash)
