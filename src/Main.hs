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
import Control.Monad (forM_, when)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (eitherReader, maybeReader, ReadM)
#endif
import SimpleCmd ((+-+), warning)
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Arch
import Cache (cacheSize, cleanEmptyCaches)
import List (listVersionsCmd)
import Paths_fedora_repoquery (version)
import Query
import Release (showReleaseCmd, downloadServer)
import Types (Mirror(..), Release (System), RepoSource(..), Verbosity(..),
              eitherRelease)

data Command = Query (Maybe String) [String]
             | CacheSize | CacheEmpties | ReleaseList

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  sysarch <- getSystemArch
  simpleCmdArgs (Just version) "fedora-repoquery tool for querying Fedora repos for packages."
    ("where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos stream), 'eln'}, with N the release version number." +-+
     "https://github.com/juhp/fedora-repoquery#readme") $
    runMain sysarch
    <$> switchWith '4' "dnf4" "Use dnf4 instead of dnf5 (if available)"
    <*> (flagWith' Quiet 'q' "quiet" "Avoid output to stderr" <|>
         flagWith Normal Verbose 'v' "verbose" "Show stderr from dnf repoquery")
    <*> switchLongWith "dynamic" "Redirect each HTTP through mirror"
    <*> switchWith 'T' "time" "Show time-stamp of repos"
    <*> (RepoSource
          <$> switchWith 'K' "koji" "Use Koji buildroot"
          <*> ((Mirror <$> strOptionWith 'm' "mirror" "URL" ("Fedora mirror [default: " ++ downloadServer ++ "]")) <|>
               flagWith DownloadFpo DlFpo 'd' "dl" "Use dl.fp.o"))
    <*> (flagWith' [Source] 's' "source" "Query source repos" <|>
         flagWith' allArchs 'A' "all-archs" "Query all (64 bit) arch repos" <|>
         many (optionWith (eitherReader eitherArch) 'a' "arch" "ARCH" ("Specify arch [default:" +-+ showArch sysarch ++ "]")))
    <*> switchWith 't' "testing" "Fedora updates-testing"
    <*> switchWith 'n' "no-query-alias" "Disable query aliases (like 'r' for '--requires')"
    <*> switchWith 'D' "debug" "Show some debug output"
    <*> (flagWith' CacheSize 'z' "cache-size" "Show total dnf repo metadata cache disksize"
         <|> flagWith' CacheEmpties 'e' "cache-clean-empty" "Remove empty dnf caches"
         <|> flagLongWith' ReleaseList "list-releases" "List Fedora versions"
         <|> Query <$> optional queryOption <*> some (strArg "[RELEASE] [REPOQUERY_OPTS]... [PACKAGE]..."))
  where
    queryOption =
      asum $ map optFlag queryCmds
      where
        optFlag o = flagLongWith' o o ("dnf option --" ++ o)

runMain :: Arch -> Bool -> Verbosity -> Bool -> Bool -> RepoSource -> [Arch]
        -> Bool -> Bool -> Bool -> Command -> IO ()
runMain sysarch dnf4 verbose dynredir time reposource archs testing noqueryalias debug command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    ReleaseList -> listVersionsCmd
    Query mopt relargs ->
      -- spanJust from utility-ht nicer but this gets us enough
      let (args,releases) = partitionEithers $ map eitherRelease relargs
      in do
        when (null releases && verbose /= Quiet) $
          warning "(using system repos)"
        forM_ (if null releases then [System] else releases) $ \release ->
          if null args
          then if null archs
               then showReleaseCmd debug dynredir reposource release sysarch Nothing testing
               else forM_ archs $ \arch -> showReleaseCmd debug dynredir reposource release sysarch (Just arch) testing
          else
            let multiple = length releases > 1 || length archs > 1
            in repoqueryCmd dnf4 debug verbose multiple dynredir time release reposource sysarch archs testing noqueryalias mopt args
