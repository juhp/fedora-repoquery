{-# LANGUAGE OverloadedStrings #-}

module Query (
  repoqueryCmd,
  ) where

import Control.Monad.Extra
import Data.Char (isSpace)
import qualified Data.List.Extra as L
import Data.Maybe (isJust, fromMaybe)
import Safe (lastMay)
import SimpleCmd (cmdLines)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath (takeBaseName)

import Arch
import Common (warning)
import Release (getRelease)
import Types
import URL (FileDir(Dir), URL, renderUrl)

-- from dnf5 repoquery.cpp pkg_attrs_options
pkgAttrsOptions :: [String]
pkgAttrsOptions =
  map ("--" ++)
  [
    "conflicts",
    "depends",
    "enhances",
    "obsoletes",
    "provides",
    "recommends",
    "requires",
    "requires_pre",
    "suggests",
    "supplements"
  ]

-- FIXME error if no testing repo
repoqueryCmd :: Bool -> Bool -> Verbosity -> Bool -> Bool -> Bool -> Release
             -> RepoSource -> Arch -> [Arch] -> Bool -> [String] -> IO ()
repoqueryCmd dnf4 debug verbose multiple dynredir checkdate release reposource sysarch archs testing args = do
  forM_ (if null archs then [sysarch] else archs) $ \arch -> do
    repoConfigs <-
      if release == System
      then return []
      else getRelease debug dynredir True checkdate reposource release sysarch (Just arch) testing
    let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","--qf","--queryformat", "--changelog"] ++ pkgAttrsOptions) args
    -- dnf5 writes repo update output to stdout
    -- https://github.com/rpm-software-management/dnf5/issues/1361
    -- but seems to cache better
    mdnf5 <- if dnf4 then return Nothing else findExecutable "dnf5"
    let queryformat =
          "%{name}-%{version}-%{release}.%{arch} (%{repoid})" ++
          if isJust mdnf5 then "\n" else ""
    -- LANG=C.utf8
    rhsm <- doesFileExist "/etc/dnf/plugins/subscription-manager.conf"
    let cmdargs = "repoquery" :
                  -- for dnf5 does not suppress repodata downloading
                  ["--quiet" | verbose /= Verbose] ++
                  -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
                  ["--disableplugin=subscription-manager" | rhsm] ++
                  (if qfAllowed then ["--qf", queryformat] else []) ++
                  -- drop modules for F39+
                  ["--setopt=module_platform_id=platform:" ++ show release] ++
                  concatMap renderRepoConfig repoConfigs ++
                  tweakedArgs (isJust mdnf5)
    -- FIXME drop "/usr/bin/"?
    let dnf = fromMaybe "/usr/bin/dnf-3" mdnf5
    when debug $
      warning $ unwords $ ('\n' : takeBaseName dnf) : map show cmdargs
    res <- cmdLines dnf cmdargs
    unless (null res) $ do
      unless (not checkdate || release == System || multiple) $ warning ""
      putStrLn $ L.intercalate "\n" res
  where
    tweakedArgs dnf5 =
      if not dnf5
      then args
      else tweakQf $ map tweakInfo args
      where
        -- dnf5 only has --info not -i
        tweakInfo "-i" = "--info"
        tweakInfo arg = arg

        -- dnf5 doesn't append \n to queryformat
        tweakQf as@(x:y:rest) | x `elem` ["--qf","--queryformat"] =
                                case lastMay y of
                                  Just sp -> if isSpace sp
                                             then as
                                             else x : (y ++ "\n") : rest
                                  Nothing -> as
                              | otherwise = x : tweakQf (y:rest)
        tweakQf xs = xs

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath=" ++ name ++ "," ++ renderUrl Dir url, "--repo=" ++ name, "--setopt=" ++ name ++ ".metadata_expire=6h" ]
