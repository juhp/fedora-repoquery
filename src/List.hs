{-# LANGUAGE OverloadedStrings #-}

module List (
  listVersionsCmd,
  showMinorCmd
  ) where

import Control.Monad
import qualified Data.Text.IO as T
import Network.HTTP.Directory
import SimpleCmd (error')

import Distribution.Fedora.Branch

import Common
import Query (showMinor)
import Types

listVersionsCmd :: Verbosity -> Manager -> String -> RepoSource
             -> IO ()
listVersionsCmd _verbose _mgr _server (RepoCentosStream _chan) =
  error' "listing Centos Stream versions not supported"
  --showMinorCmd mgr server (RepoCentosStream chan) X86_64
listVersionsCmd _ _ _ RepoKoji =
  error' "listing Koji versions not supported"
listVersionsCmd verbose mgr server (RepoFedora _) = do
  let url = server +/+ "fedora/linux/releases"
  unless (verbose == Quiet) $ warning $! "<" ++ url ++ ">"
  httpDirectory mgr url >>= mapM_ (T.putStrLn . noTrailingSlash)

showMinorCmd :: Branch -> Manager -> String -> RepoSource
             -> Arch -> IO ()
showMinorCmd branch mgr server reposource arch =
  void $ showMinor Normal False branch mgr server reposource arch
