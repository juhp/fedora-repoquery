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
import Control.Monad (forM_)
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (eitherReader, maybeReader, ReadM)
#endif
import SimpleCmd (cmd, (+-+))
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
  sysarch <- readArch <$> cmd "rpm" ["--eval", "%{_arch}"]
  simpleCmdArgs' (Just version) "fedora-repoquery tool for querying Fedora repos for packages."
    ("where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos stream), 'eln'}, with N the release version number." +-+
     "https://github.com/juhp/fedora-repoquery#readme") $
    runMain sysarch
    <$> switchWith '4' "dnf4" "Use dnf4 instead of dnf5 (if available)"
    <*> (flagWith' Quiet 'q' "quiet" "Avoid output to stderr" <|> flagWith Normal Verbose 'v' "verbose" "Show stderr from dnf repoquery")
    <*> (RepoSource
          <$> switchWith 'K' "koji" "Use Koji buildroot"
          <*> (flagLongWith' ChanDevel "devel-channel" "Use eln development compose" <|>
               flagLongWith ChanProd ChanTest "test-channel" "Use eln test compose [default: production]")
          <*> ((Mirror <$> strOptionWith 'm' "mirror" "URL" ("Fedora mirror [default: " ++ downloadServer ++ "]")) <|>
               flagWith DownloadFpo DlFpo 'D' "dl" "Use dl.fp.o"))
    <*> (flagWith' [Source] 's' "source" "Query source repos" <|>
         flagWith' allArchs 'A' "all-archs" "Query all (64 bit) arch repos" <|>
         many (optionWith (eitherReader eitherArch) 'a' "arch" "ARCH" ("Specify arch [default:" +-+ showArch sysarch ++ "]")))
    <*> switchWith 't' "testing" "Fedora updates-testing"
    <*> switchWith 'd' "debug" "Show some debug output"
    <*> (flagWith' CacheSize 'z' "cache-size" "Show total dnf repo metadata cache disksize"
         <|> flagWith' CacheEmpties 'e' "cache-clean-empty" "Remove empty dnf caches"
         <|> flagWith' List 'l' "list" "List Fedora versions"
         <|> Query <$> argumentWith releaseM "RELEASE" <*> many (strArg "[REPOQUERY_OPTS] [PACKAGE]..."))
  where
    releaseM :: ReadM Release
    releaseM = maybeReader readRelease

runMain :: Arch -> Bool -> Verbosity -> RepoSource -> [Arch] -> Bool -> Bool
        -> Command -> IO ()
runMain sysarch dnf4 verbose reposource archs testing debug command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    List -> listVersionsCmd
    Query release args -> do
      if null args
        then if null archs
             then showReleaseCmd debug reposource release sysarch Nothing testing
             else forM_ archs $ \arch -> showReleaseCmd debug reposource release sysarch (Just arch) testing
      else repoqueryCmd dnf4 debug verbose release reposource sysarch archs testing args
