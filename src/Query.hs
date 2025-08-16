{-# LANGUAGE OverloadedStrings #-}

module Query (
  repoqueryCmd,
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

queryAliases :: [([Char], [Char])]
queryAliases =
  -- dnf5 only has --info not -i
  [ ("-i", "info")
  , ("i", "info")
  , ("l", "list")
  , ("r", "requires")
  , ("wr", "whatrequires")
  , ("p", "provides")
  , ("wp", "whatprovides")
  , ("d", "depends")
  , ("wd", "whatdepends")
  , ("rec", "recommends")
  , ("wrec", "whatrecommends")
  , ("sug", "suggests")
  , ("wsug", "whatsuggests")
  , ("c", "conflicts")
  , ("wc", "whatconflicts")
  , ("enh", "enhances")
  , ("wenh", "whatenhances")
  , ("o", "obsoletes")
  , ("wo", "whatobsoletes")
  , ("sup", "supplements")
  , ("wsup", "whatsupplements")
  , ("qf", "queryformat")
  , ("latest", "latest-limit")
  ]

repoqueryCmd :: Bool -> Bool -> Verbosity -> Bool -> Bool -> Bool -> Release
             -> RepoSource -> Arch -> [Arch] -> Bool -> Bool -> [String]
             -> IO ()
repoqueryCmd dnf4 debug verbose multiple dynredir checkdate release reposource sysarch archs testing noqueryalias args = do
  forM_ (if null archs then [sysarch] else archs) $ \arch -> do
    repoConfigs <-
      if release == System
      then return []
      else getRelease debug dynredir True checkdate reposource release sysarch (Just arch) testing
    let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","--qf","--queryformat", "--changelog"] ++ pkgAttrsOptions) tweakedArgs
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
                  (if qfAllowed then ["--qf", queryformat] else []) ++
                  -- drop modules for F39+
                  ["--setopt=module_platform_id=platform:" ++ show release] ++
                  concatMap renderRepoConfig repoConfigs ++
                  tweakQfOpt (isJust mdnf5) tweakedArgs
    -- FIXME drop "/usr/bin/"?
    let dnf = fromMaybe "/usr/bin/dnf-3" mdnf5
    when debug $
      warning $ unwords $ ('\n' : takeBaseName dnf) : map show cmdargs
    res <- cmdLines dnf cmdargs
    unless (null res) $ do
      unless (not checkdate || release == System || multiple) $ warning ""
      putStrLn $ intercalate "\n" res
  where
    tweakedArgs = tweakArgs args
      where
        tweakArgs [] = []
        tweakArgs (h:t) =
          if noqueryalias then h : t else tweakArg h : t

        tweakArg o =
          case lookup o queryAliases of
            Just oname -> "--" ++ oname
            Nothing ->
              if o `elem` map snd queryAliases
              then "--" ++ o
              else o

    -- dnf5 doesn't append \n to queryformat
    tweakQfOpt dnf5 xs =
      if dnf5 then
        case xs of
          (x:y:rest) | x `elem` ["--qf","--queryformat"] -> x : tweakQf y : rest
                     | otherwise -> x : tweakQfOpt dnf5 (y:rest)
          _ -> xs
      else xs
      where
        tweakQf qf =
          expandQf qf ++
          case lastMay qf of
            Just lc -> if isSpace lc then "" else "\n"
            Nothing -> ""

        expandQf qf =
          case lower qf of
            "n" -> "%{name}"
            "nv" -> "%{name}-%{version}"
            "nvr" -> expandQf "nv" ++ "-%{release}"
            "nvra" -> expandQf "nvr" <.> "%{arch}"
            "envr" -> "%{epoch}:" ++ expandQf "nvr"
            "envra" -> "%{epoch}:" ++ expandQf "nvra"
            _ -> qf

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath=" ++ name ++ "," ++ renderUrl Dir url,
   "--repo=" ++ name,
   "--setopt=" ++ name ++ ".metadata_expire=6h",
   -- avoid failing silently for 404 etc
   "--setopt=" ++ name ++ ".skip_if_unavailable=0"]
