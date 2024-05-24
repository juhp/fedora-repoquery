{-# LANGUAGE OverloadedStrings #-}

module Query (
  repoqueryCmd,
  ) where

import Control.Monad.Extra
import qualified Data.List.Extra as L
import Data.Maybe (isJust, fromMaybe)
import SimpleCmd (cmdLines)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath (takeBaseName)

import Arch
import Common (warning)
import ShowRelease (showRelease)
import Types
import URL (URL, renderUrl)

-- FIXME --no-redirect?
-- FIXME error if no testing repo
repoqueryCmd :: Bool -> Bool -> Verbosity -> Release -> RepoSource -> Arch
             -> [Arch] -> Bool -> [String] -> IO ()
repoqueryCmd dnf4 debug verbose release reposource sysarch archs testing args = do
  forM_ (if null archs then [sysarch] else archs) $ \arch -> do
    repoConfigs <- showRelease debug verbose True reposource release sysarch (Just arch) testing
    let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","-qf","--queryformat", "--changelog", "--provides", "--requires"]) args
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
                  ["--quiet" | verbose /= Verbose] ++
                  -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
                  ["--disableplugin=subscription-manager" | rhsm] ++
                  (if qfAllowed then ["--qf", queryformat] else []) ++
                  -- drop modules for F39+
                  ["--setopt=module_platform_id=platform:" ++ show release] ++
                  concatMap renderRepoConfig repoConfigs ++
                  args
    -- FIXME drop "/usr/bin/"?
    let dnf = fromMaybe "/usr/bin/dnf-3" mdnf5
    when debug $
      warning $ unwords $ ('\n' : takeBaseName dnf) : map show cmdargs
    res <- cmdLines dnf cmdargs
    unless (null res) $ do
      unless (verbose == Quiet) $ warning ""
      putStrLn $ L.intercalate "\n" res

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath=" ++ name ++ "," ++ renderUrl url, "--repo=" ++ name, "--setopt=" ++ name ++ ".metadata_expire=6h" ]
