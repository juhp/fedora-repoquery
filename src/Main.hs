{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

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
import Options.Applicative (eitherReader)
#endif
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Distribution.Fedora.Branch
--import Distribution.Fedora.Repoquery
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (maybeReader, ReadM)
#endif

import Cache
import List
import Paths_fedora_repoquery (version)
import Query
import Types

data Command = Query Branch [String] | CacheSize | CacheEmpties | List

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs' (Just version) "Fedora repoquery tool"
    "Tool for querying Fedora repos for packages." $
    runMain
    <$> (flagWith' Quiet 'q' "quiet" "Avoid output to stderr" <|> flagWith Normal Verbose 'v' "verbose" "Show stderr from dnf repoquery")
    <*> (flagWith' RepoKoji 'K' "koji" "Use Koji buildroot" <|>
         (flagWith' RepoCentosStream 'C' "centos-stream" "Use Centos Stream repo" <*> switchWith 'D' "devel" "Use centos-stream development compose (default is test)") <|>
         ((RepoFedora <$> ((Mirror <$> strOptionWith 'm' "mirror" "URL" ("Fedora mirror [default: " ++ downloadServer ++ "]")) <|> flagWith DownloadFpo DlFpo 'd' "dl" "Use dl.fp.o"))))
    <*> (flagWith' Source 's' "source" "Query source repos" <|>
         optionalWith (eitherReader readArch) 'a' "arch" "ARCH" "Specify arch [default: x86_64]" X86_64)
    <*> (flagWith' CacheSize 'z' "cache-size" "Show total dnf repo metadata cache disksize"
         <|> flagWith' CacheEmpties 'e' "cache-clean-empty" "Remove empty dnf caches"
         <|> flagWith' List 'l' "list" "List versions"
         <|> Query <$> argumentWith branchM "RELEASE" <*> many (strArg "[REPOQUERY_OPTS] [PACKAGE/KEY]..."))
  where
    branchM :: ReadM Branch
    branchM = maybeReader readBranch

runMain :: Verbosity -> RepoSource
        -> Arch -> Command -> IO ()
runMain verbose reposource arch command = do
  case command of
    CacheSize -> cacheSize
    CacheEmpties -> cleanEmptyCaches
    List -> listVersionsCmd verbose reposource
    Query branch args -> do
      if null args
      then showReleaseCmd branch reposource arch
      else repoqueryCmd verbose branch reposource arch args
