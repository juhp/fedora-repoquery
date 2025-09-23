{-# LANGUAGE OverloadedStrings #-}

module Query (
  repoqueryCmd,
  DnfOption(..)
  ) where

import Control.Monad.Extra
import Data.Char (isSpace)
import Data.List.Extra
import Data.Maybe (isJust, fromMaybe)
import Safe (lastMay)
import SimpleCmd (cmdLines)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath (takeBaseName, (<.>))

import Arch
import Common (warning)
import Release (getRelease)
import Types
import URL (FileDir(Dir), URL, renderUrl)

data DnfOption =
  DnfFlag String | DnfOption String String
  deriving Eq

renderOption :: DnfOption -> String
renderOption (DnfFlag o) = '-' : '-' : o
renderOption (DnfOption o v) = '-' : '-' : o ++ '=' : v

noQueryFormatOptions :: [String]
noQueryFormatOptions =
  [
    -- from dnf5 repoquery.cpp pkg_attrs_options
    "conflicts",
    "depends",
    "enhances",
    "obsoletes",
    "provides",
    "recommends",
    "requires",
    "require-pre",
    "suggests",
    "supplements",
    -- others conflicting with '--qf'
    "info",
    "list",
    "location",
    "source",
    "queryformat",
    "changelogs"
  ]

repoqueryCmd :: Bool -> Bool -> Verbosity -> Bool -> Bool -> Bool -> Release
             -> RepoSource -> Arch -> [Arch] -> Bool -> [DnfOption]
             -> [String] -> IO ()
repoqueryCmd dnf4 debug verbose multiple dynredir checkdate release reposource sysarch archs testing opts args = do
  forM_ (if null archs then [sysarch] else archs) $ \arch -> do
    repoConfigs <-
      if release == System
      then return []
      else getRelease debug dynredir True checkdate reposource release sysarch (Just arch) testing
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
                  ["--quiet" | verbose /= Verbose || not debug] ++
                  -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
                  ["--disableplugin=subscription-manager" | rhsm] ++
                  ["--queryformat=" ++ queryformat | queryFormatAllowed] ++
                  -- drop modules for F39+
                  ["--setopt=module_platform_id=platform:" ++ show release] ++
                  concatMap renderRepoConfig repoConfigs ++
                  map (renderOption . tweakQfOpt (isJust mdnf5)) opts ++ args
    -- FIXME drop "/usr/bin/"?
    let dnf = fromMaybe "/usr/bin/dnf-3" mdnf5
    when debug $
      warning $ unwords $ ('\n' : takeBaseName dnf) : map show cmdargs
    res <- cmdLines dnf cmdargs
    unless (null res) $ do
      unless (not checkdate || release == System || multiple) $ warning ""
      putStrLn $ intercalate "\n" res
  where
    queryFormatAllowed =
      not $ any (`elem` map DnfFlag noQueryFormatOptions) opts

    tweakQfOpt dnf5 opt =
        case opt of
          DnfOption "queryformat" fv -> DnfOption "queryformat" (tweakQf fv)
          _ -> opt
      where
        -- dnf5 doesn't append \n to queryformat
        tweakQf fv =
          expandQf fv ++
          case lastMay fv of
            Just lc | dnf5 -> if isSpace lc then "" else "\n"
            _ -> ""

        expandQf qf =
          case lower qf of
            "n" -> "%{name}"
            "nv" -> "%{name}-%{version}"
            "nvr" -> expandQf "nv" ++ "-%{release}"
            "nvra" -> expandQf "nvr" <.> "%{arch}"
            -- fixme? check order correct for epoch
            "envr" -> "%{epoch}:" ++ expandQf "nvr"
            "envra" -> "%{epoch}:" ++ expandQf "nvra"
            "default" -> "%{full_nevra}"
            _ -> qf

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath=" ++ name ++ "," ++ renderUrl Dir url,
   "--repo=" ++ name,
   "--setopt=" ++ name ++ ".metadata_expire=6h",
   -- avoid failing silently for 404 etc
   "--setopt=" ++ name ++ ".skip_if_unavailable=0"]
