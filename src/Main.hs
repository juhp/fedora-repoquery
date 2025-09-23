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
import Control.Monad (forM_, unless, when)
import Data.Either (rights, partitionEithers)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Options.Applicative (flag', long, short, strOption)
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (eitherReader, maybeReader, ReadM)
#endif
import SimpleCmd ((+-+), error', warning)
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Arch
import Cache (cacheSize, cleanEmptyCaches)
import List (listVersionsCmd)
import Paths_fedora_repoquery (version)
import Query
import Release (activeFedoraReleases, downloadServer, releaseBranch,
                showReleaseCmd)
import Types (Mirror(..), Release (System), RepoSource(..), Verbosity(..),
              eitherRelease)

data Command = Query [DnfOption] [String]
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
    -- FIXME: --all-epel and --all-releases
    <*> switchWith 'F' "all-fedora" "Query all Fedora releases"
    <*> (flagWith' [Source] 's' "source" "Query source repos" <|>
         flagWith' allArchs 'A' "all-archs" "Query all (64 bit) arch repos" <|>
         many (optionWith (eitherReader eitherArch) 'a' "repo-arch" "ARCH" ("Specify repo arch [default:" +-+ showArch sysarch ++ "]")))
    <*> switchWith 't' "testing" "Fedora updates-testing"
    <*> switchWith 'D' "debug" "Show some debug output"
    <*> (flagWith' CacheSize 'z' "cache-size" "Show total dnf repo metadata cache disksize"
         <|> flagWith' CacheEmpties 'e' "cache-clean-empty" "Remove empty dnf caches"
         <|> flagLongWith' ReleaseList "list-releases" "List Fedora versions"
         <|> Query <$> many queryOptions <*> many (strArg "[RELEASE]... [REPOQUERY_OPTS]... [PKGSPECIFIER]..."))

-- man 8 dnf-repoquery (dnf5-5.2.17.0-2.fc44)
-- https://github.com/rpm-software-management/dnf5/blob/main/doc/commands/repoquery.8.rst

dnfFlag, dnfOption :: String -> Parser DnfOption
dnfFlag o = flag' (DnfFlag o) (long o)
dnfOption o = DnfOption o <$> strOption (long o)
dnfFlag', dnfOption' :: Char -> String -> Parser DnfOption
dnfFlag' s l = flag' (DnfFlag l) (short s <> long l)
dnfOption' s l = DnfOption l <$> strOption (short s <> long l)
dnfOption'' :: String -> String -> Parser DnfOption
dnfOption'' s l = DnfOption l <$> strOption (long s <> long l)

queryOptions :: Parser DnfOption
queryOptions =
  -- # OPTIONS
  dnfOption "advisories"
  <|> dnfOption "advisory-severities"
  <|> dnfOption "arch"
  <|> dnfFlag "available" -- default
  <|> dnfFlag "bugfix"
  <|> dnfOption "bzs"
  <|> dnfOption "cves"
  <|> dnfFlag "disable-modular-filtering"
  <|> dnfFlag "duplicates"
  <|> dnfFlag "enhancement"
  <|> dnfFlag "exactdeps"
  <|> dnfFlag "extras"
  <|> dnfOption' 'f' "file"
  <|> dnfFlag "installed"
  <|> dnfOption "installed-from-repo"
  <|> dnfFlag "installonly"
  <|> dnfOption "latest-limit"
  <|> dnfFlag "leaves"
  <|> dnfFlag "newpackage"
  <|> dnfOption "providers-of" -- for certain commands see manpage
  <|> dnfFlag "recent"
  <|> dnfFlag "recursive" -- with --whatrequires or --providers-of only
  <|> dnfFlag "security"
  <|> dnfFlag "srpm"
  <|> dnfFlag "unneeded"
  <|> dnfFlag "upgrades"
  <|> dnfFlag "userinstalled"
  <|> dnfOption "whatconflicts"
  <|> dnfOption "whatdepends"
  <|> dnfOption "whatenhances"
  <|> dnfOption "whatobsoletes"
  <|> dnfOption "whatprovides"
  <|> dnfOption "whatrecommends"
  <|> dnfOption "whatrequires"
  <|> dnfOption "whatsuggests"
  <|> dnfOption "whatsupplements"
  -- # FORMATTING
  <|> dnfFlag "conflicts"
  <|> dnfFlag "depends"
  <|> dnfFlag "enhances"
  <|> dnfFlag "files"
  <|> dnfFlag' 'l' "list"
  <|> dnfFlag "obsoletes"
  <|> dnfFlag "provides"
  <|> dnfFlag "recommends"
  <|> dnfFlag "requires"
  <|> dnfFlag "requires-pre"
  <|> dnfFlag "sourcerpm"
  <|> dnfFlag "suggests"
  <|> dnfFlag "supplements"
  <|> dnfFlag "location"
  <|> dnfFlag' 'i' "info"
  <|> dnfFlag "changelogs" -- map --changelog
  <|> dnfFlag "querytags"
  <|> dnfOption'' "qf" "queryformat"

runMain :: Arch -> Bool -> Verbosity -> Bool -> Bool -> RepoSource -> Bool
        -> [Arch] -> Bool -> Bool -> Command -> IO ()
runMain sysarch dnf4 verbose dynredir time reposource allreleases archs testing debug command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    ReleaseList -> listVersionsCmd
    Query opts relargs ->
      -- spanJust from utility-ht nicer but this gets us enough
      let (args,releases) = partitionEithers $ map eitherRelease relargs
      in do
        unless (null releases || not allreleases) $
          error' "cannot specify releases and --all-releases"
        when (null releases && not allreleases) $
          when (verbose == Verbose || debug) $
          warning "(using system repos)"
        releaselist <-
          if null releases
          then
            if allreleases
            then do
              brels <- activeFedoraReleases
              return $ rights $ map (eitherRelease . releaseBranch) brels
            else return [System]
          else return releases
        forM_ releaselist $ \release ->
          if null args
          then if null archs
               then showReleaseCmd debug dynredir reposource release sysarch Nothing testing
               else forM_ archs $ \arch -> showReleaseCmd debug dynredir reposource release sysarch (Just arch) testing
          else
            let multiple = length releaselist > 1 || length archs > 1
            in repoqueryCmd dnf4 debug verbose multiple dynredir time release reposource sysarch archs testing opts args
