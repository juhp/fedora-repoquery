{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

module Main (main) where

#if !MIN_VERSION_simple_cmd_args(0,1,4)
import Control.Applicative (
  many,
#if !MIN_VERSION_simple_cmd_args(0,1,3)
  (<|>)
#endif
  )
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (eitherReader, maybeReader, ReadM)
#endif
import SimpleCmd ((+-+))
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

--import Distribution.Fedora.Repoquery

import Cache
import List
import Paths_fedora_repoquery (version)
import Query
import Types

data Command = Query Release [String] | CacheSize | CacheEmpties | List

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "fedora-repoquery tool for querying Fedora repos for packages."
    ("where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos stream), 'eln'}, with N the release version number." +-+
     "https://github.com/juhp/fedora-repoquery#readme") $
    runMain
    <$> (flagWith' Quiet 'q' "quiet" "Avoid output to stderr" <|> flagWith Normal Verbose 'v' "verbose" "Show stderr from dnf repoquery")
    <*> (RepoSource
          <$> switchWith 'K' "koji" "Use Koji buildroot"
          <*> (flagLongWith' CentosDevel "centos-devel" "Use centos-stream development compose" <|>
               flagLongWith CentosProd CentosTest "centos-test" "Use centos-stream test compose [default: production]")
          <*> ((Mirror <$> strOptionWith 'm' "mirror" "URL" ("Fedora mirror [default: " ++ downloadServer ++ "]")) <|>
               flagWith DownloadFpo DlFpo 'D' "dl" "Use dl.fp.o"))
    <*> (flagWith' Source 's' "source" "Query source repos" <|>
         optionalWith (eitherReader readArch) 'a' "arch" "ARCH" "Specify arch [default: x86_64]" X86_64)
    <*> switchWith 'd' "debug" "Show some debug output"
    <*> (flagWith' CacheSize 'z' "cache-size" "Show total dnf repo metadata cache disksize"
         <|> flagWith' CacheEmpties 'e' "cache-clean-empty" "Remove empty dnf caches"
         <|> flagWith' List 'l' "list" "List Fedora versions"
         <|> Query <$> argumentWith releaseM "RELEASE" <*> many (strArg "[REPOQUERY_OPTS] [PACKAGE]..."))
  where
    releaseM :: ReadM Release
    releaseM = maybeReader readRelease

runMain :: Verbosity -> RepoSource -> Arch -> Bool -> Command -> IO ()
runMain verbose reposource arch debug command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    List -> listVersionsCmd verbose reposource
    Query release args -> do
      if null args
      then showReleaseCmd debug reposource release arch
      else repoqueryCmd debug verbose release reposource arch args
