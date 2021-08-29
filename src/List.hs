{-# LANGUAGE OverloadedStrings #-}

module List (
  listMajorCmd,
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

listMajorCmd :: Verbosity -> Branch -> Manager -> String -> RepoSource
             -> IO ()
listMajorCmd _verbose branch mgr server (RepoCentosStream chan) =
  showMinorCmd branch mgr server (RepoCentosStream chan) X86_64
listMajorCmd _ _ _ _ RepoKoji =
  error' "listing Koji versions not supported"
listMajorCmd verbose branch mgr server (RepoFedora _) = do
  let url = server +/+ show branch
  unless (verbose == Quiet) $ warning $! "<" ++ url ++ ">"
  httpDirectory mgr url >>= mapM_ (T.putStrLn . noTrailingSlash)

showMinorCmd :: Branch -> Manager -> String -> RepoSource
             -> Arch -> IO ()
showMinorCmd branch mgr server reposource arch =
  void $ showMinor Normal False branch mgr server reposource arch
