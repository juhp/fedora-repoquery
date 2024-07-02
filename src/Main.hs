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
import Data.List.HT (spanJust)
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (eitherReader, maybeReader, ReadM)
#endif
import SimpleCmd ((+-+))
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Arch
import Cache (cacheSize, cleanEmptyCaches)
import List (listVersionsCmd)
import Paths_fedora_repoquery (version)
import Query (repoqueryCmd)
import ShowRelease (showReleaseCmd, downloadServer)
import Types (Channel(..), Mirror(..), RepoSource(..), Verbosity(..),
              readRelease)

data Command = Query [String] | CacheSize | CacheEmpties | List

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  sysarch <- getSystemArch
  simpleCmdArgs' (Just version) "fedora-repoquery tool for querying Fedora repos for packages."
    ("where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos stream), 'eln'}, with N the release version number." +-+
     "https://github.com/juhp/fedora-repoquery#readme") $
    runMain sysarch
    <$> switchWith '4' "dnf4" "Use dnf4 instead of dnf5 (if available)"
    <*> (flagWith' Quiet 'q' "quiet" "Avoid output to stderr" <|> flagWith Normal Verbose 'v' "verbose" "Show stderr from dnf repoquery")
    <*> switchLongWith "quick" "Skip http repo url checks"
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
         <|> Query <$> some (strArg "RELEASE... [REPOQUERY_OPTS]... [PACKAGE]..."))

runMain :: Arch -> Bool -> Verbosity -> Bool -> RepoSource -> [Arch] -> Bool -> Bool
        -> Command -> IO ()
runMain sysarch dnf4 verbose quick reposource archs testing debug command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    List -> listVersionsCmd
    Query relargs ->
      let (releases,args) = spanJust readRelease relargs
      in
        forM_ releases $ \release ->
        if null args
        then if null archs
             then showReleaseCmd debug reposource release sysarch Nothing testing
             else forM_ archs $ \arch -> showReleaseCmd debug reposource release sysarch (Just arch) testing
        else
          let multiple = length releases > 1 || length archs > 1
          in repoqueryCmd dnf4 debug verbose (quick || multiple) release reposource sysarch archs testing args
