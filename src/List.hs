{-# LANGUAGE OverloadedStrings #-}

module List (
  listVersionsCmd
  ) where

import Control.Monad
import qualified Data.Text.IO as T
import Network.HTTP.Directory
--import SimpleCmd (error')

import Common
import Query (getFedoraServer, fedoraTop)
import Types
import URL

listVersionsCmd :: Bool -> Verbosity -> RepoSource -> Arch -> IO ()
-- listVersionsCmd _verbose _reposource =
--   error' "listing Centos Stream versions not supported"
--   --showReleaseCmd mgr server (RepoCentosStream chan) arch
-- listVersionsCmd _ RepoKoji =
--   error' "listing Koji versions not supported"
listVersionsCmd debug verbose reposource arch = do
  mgr <- httpManager
  -- FIXME handle non-fedora versions (eg epel)
  -- FIXME no longer releases/, but parent dir
  (url,_) <- getFedoraServer debug mgr reposource (fedoraTop arch) ["releases"]
  let rurl = renderUrl url
  unless (verbose == Quiet) $ warning $! "<" ++ rurl ++ ">"
  -- FIXME filter very old releases
  httpDirectory mgr rurl >>= mapM_ (T.putStrLn . noTrailingSlash)
