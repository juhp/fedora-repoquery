{-# LANGUAGE CPP, OverloadedStrings #-}

module Query (
  getFedoraServer,
  repoqueryCmd,
  showReleaseCmd,
  downloadServer,
  fedoraTop
  ) where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import Data.Time.LocalTime (utcToLocalZonedTime)
import Fedora.Bodhi
import Network.HTTP.Directory
import SimpleCmd
import System.Cached.JSON
import System.Directory (doesFileExist)
import Text.Regex

#if !MIN_VERSION_simple_cmd(0,2,0)
import Common (warning)
#endif
import Types
import URL

showReleaseCmd :: Bool -> RepoSource -> Release  -> Arch -> Maybe Arch -> Bool
               -> IO ()
showReleaseCmd debug reposource release sysarch march testing =
  void $ showRelease debug Normal False reposource release sysarch march testing

repoqueryCmd :: Bool -> Verbosity -> Release -> RepoSource -> Arch
             -> Maybe Arch -> Bool -> [String] -> IO ()
repoqueryCmd debug verbose release reposource sysarch march testing args = do
  repoConfigs <- showRelease debug verbose True reposource release sysarch march testing
  let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","-qf","--queryformat"]) args
      queryformat = "%{name}-%{version}-%{release}.%{arch} (%{repoid})"
  -- LANG=C.utf8
  rhsm <- doesFileExist "/etc/dnf/plugins/subscription-manager.conf"
  let cmdargs = "repoquery" :
                ["--quiet" | verbose /= Verbose] ++
                -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
                ["--disableplugin=subscription-manager" | rhsm] ++
                (if qfAllowed then ["--qf", queryformat] else []) ++
                ["--setopt=module_platform_id=platform:" ++ show release] ++
                concatMap renderRepoConfig repoConfigs ++
                args
  when debug $
    warning $ unwords $ "\ndnf" : map show cmdargs
  res <- cmdLines "dnf" cmdargs
  unless (null res) $ do
    unless (verbose == Quiet) $ warning ""
    putStrLn $ L.intercalate "\n" res

-- majorVersion :: Release -> String
-- majorVersion (Fedora n) = show n
-- majorVersion Rawhide = "rawhide"

repoVersion :: Release -> String -> String
repoVersion Rawhide _ = "fedora-rawhide"
repoVersion release repo =
  show release ++
  case release of
    Centos _ -> '-':repo
    ELN -> '-':repo
    EPEL _ -> if repo == "epel-testing" then "-testing" else ""
    Fedora _ | repo /= "releases" -> '-':repo
    _ -> ""

repoConfigArgs :: RepoSource -> Arch -> Maybe Arch -> Release -> (String,URL)
               -> (String,(URL,[String]))
-- non-koji
repoConfigArgs (RepoSource False _chan mirror) sysarch march release (repo,url) =
  let arch = fromMaybe sysarch march
      archsuffix = if arch == sysarch then "" else "-" ++ showArch arch
      reponame = repoVersion release repo ++ archsuffix ++
                 case mirror of
                   DownloadFpo -> ""
                   Mirror serv ->
                     '-' : subRegex (mkRegex "https?://") serv ""
                   DlFpo -> "-dl.fpo"
      path =
        case release of
          Centos _ -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          ELN -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          EPEL n -> (if n >= 7 then ("Everything" :) else id) [showArch arch]
          EPELNext _n -> ["Everything", showArch arch]
          Fedora _ -> ["Everything", showArch arch] ++ (if arch == Source then ["tree"] else ["os" | repo == "releases"])
          Rawhide -> ["Everything", showArch arch, if arch == Source then "tree" else "os"]
  in (reponame, (url, path))
-- koji
repoConfigArgs (RepoSource True _chan _mirror) sysarch march release (repo,url) =
  let (compose,path) =
        case release of
          Rawhide -> (["repos", show release, "latest"],"")
          _ -> (["repos", show release ++ "-build/latest"],"")
      arch = fromMaybe sysarch march
      reponame = repo ++ "-" ++ show release ++ "-build" ++
                 if arch == sysarch then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, ""]))
-- repoConfigArgs url (RepoCentosStream chan) arch release repo =
--   let (compose,path) = (["composes", channel chan, "latest-CentOS-Stream", "compose"], repo)
--       reponame = repo ++ "-Centos-" ++ show release ++ "-Stream" ++ "-" ++ show chan ++ if arch == sysarch then "" else "-" ++ showArch arch
--   in (reponame, (url +//+ compose, [path, showArch arch, "os/"]))

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath", name ++ "," ++ renderUrl url, "--repo", name]

showRelease :: Bool -> Verbosity -> Bool -> RepoSource -> Release -> Arch
            -> Maybe Arch -> Bool -> IO [(String, URL)]
showRelease debug verbose warn reposource@(RepoSource koji _chan _mirror) release sysarch march testing = do
  mgr <- httpManager
  let arch = fromMaybe sysarch march
  (url,path) <- getURL debug mgr reposource release arch
  let urlpath = url +//+ path
  when debug $ putStrLn $ renderUrl urlpath
  repos <-
    case release of
      -- RepoKoji -> ["koji-fedora"]
      Centos n -> return [("BaseOS",urlpath), ("AppStream",url), (if n >= 9 then "CRB" else "PowerTools",url)]
      ELN -> return [("BaseOS",urlpath), ("AppStream",urlpath), ("CRB",urlpath)]
      Rawhide -> return [("development", urlpath)]
      Fedora n -> do
        pending <- pendingFedoraRelease n
        return $
          if pending
          then [("development", urlpath)]
          else ("releases", urlpath) :
               ("updates", url +//+ ["updates",show n]) :
               [("updates-testing", url +//+ ["updates","testing",show n]) | testing]
      EPEL n -> return $
                 ("epel",urlpath) :
                 [("epel-testing",url +//+ ["testing", show n]) | testing]
      EPELNext _n -> return [("epelnext",urlpath)]
  forM repos $ \repourl -> do
    let (reponame,(url',path')) = repoConfigArgs reposource sysarch march release repourl
        baserepo = url' +//+ path'
    when debug $ do
      putStrLn $ "url" +-+ renderUrl url'
      putStrLn $ renderUrl baserepo
    ok <- httpExists mgr $ trailingSlash $ renderUrl baserepo
    if ok
      then do
      unless (verbose == Quiet) $ do
        mtime <- do
          let composeinfo =
                if koji
                then url' +//+ ["repo.json"]
                else
                  case release of
                    Centos _ -> url' +//+ ["COMPOSE_ID"] -- ["metadata","composeinfo.json"]
                    ELN -> url' +//+ ["metadata","composeinfo.json"]
                    EPEL _ -> url' +//+ ["Everything", "state"]
                    EPELNext _ -> url' +//+ ["Everything", "state"]
                    Fedora _ -> url' +//+
                                if "updates" `L.isSuffixOf` reponame ||
                                   "updates-testing" `L.isSuffixOf` reponame
                                then ["Everything", "state"]
                                else ["COMPOSE_ID"]
                    Rawhide -> url' +//+ ["COMPOSE_ID"]
          when debug $ putStrLn $ renderUrl composeinfo
          exists <- httpExists mgr (renderUrl composeinfo)
          if exists
            then httpLastModified mgr (renderUrl composeinfo)
            else return Nothing
        whenJust mtime $ \utc -> do
          date <- utcToLocalZonedTime utc
          (if warn then warning else putStrLn) $ show date ++ " <" ++ renderUrl url' ++ ">"
      return (reponame, url' +//+ path')
      else
        error' $ renderUrl baserepo ++ " not found"

downloadServer :: String
downloadServer = "https://download.fedoraproject.org/pub"

fedoraTop :: Arch -> [String]
fedoraTop arch =
  -- FIXME support older archs
  if arch `elem` [PPC64LE, S390X]
  then ["fedora-secondary"]
  else ["fedora", "linux"]

epelTop :: [String]
epelTop = ["epel"]

getURL :: Bool -> Manager -> RepoSource -> Release -> Arch -> IO (URL,[String])
getURL debug mgr reposource@(RepoSource koji chan _mirror) release arch =
  case release of
    Centos 9 ->
      let url = URL $
            if koji
            then "https://odcs.stream.centos.org"
            else "https://mirror.stream.centos.org/9-stream/"
      in return (url,[])
    Centos 8 ->
      return (URL "http://mirror.centos.org/centos/8-stream/", [])
    Centos _ -> error' "old Centos is not supported yet"
    ELN ->
      return (URL "https://odcs.fedoraproject.org/composes", [channel chan, "latest-Fedora-ELN", "compose"])
    EPEL n | n < 7 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive/epel", [show n])
    EPEL n -> getFedoraServer debug mgr reposource epelTop [show n]
    EPELNext n -> getFedoraServer debug mgr reposource (epelTop ++ ["next"]) [show n]
    -- FIXME hardcoded
    Fedora n | n < 36 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive" +//+ fedoraTop arch, ["releases", show n])
    -- FIXME handle rawhide version
    Fedora n -> do
      pending <- pendingFedoraRelease n
      getFedoraServer debug mgr reposource (fedoraTop arch)
        [if pending then "development" else "releases", show n]
    Rawhide -> getFedoraServer debug mgr reposource (fedoraTop arch) ["development", "rawhide"]

pendingFedoraRelease :: Natural -> IO Bool
pendingFedoraRelease n = do
  pending <- mapMaybe (lookupKey "branch") <$> getCachedJSONQuery "fedora-bodhi-releases-pending" "fedora-bodhi-releases-pending" (bodhiReleases (makeKey "state" "pending")) 1000
  return $ show (Fedora n) `elem` pending

getFedoraServer :: Bool -> Manager -> RepoSource -> [String] -> [String]
                -> IO (URL,[String])
getFedoraServer debug mgr (RepoSource koji _ mirror) top path =
  if koji
  then return (URL "https://kojipkgs.fedoraproject.org",[])
  else
    case mirror of
      DownloadFpo -> do
        let url = URL downloadServer +//+ top ++ path
            rurl = renderUrl url
        redir <- httpRedirect mgr rurl
        case redir of
          Nothing -> do
            warning $ "no redirect for" +-+ rurl
            return (URL downloadServer +//+ top,path)
          Just actual -> do
            when debug $ do
              warning rurl
              warning $ "redirected to" +-+ show actual
            return (URL $ removeSubpath path $ B.unpack actual, path)
      -- FIXME how to handle any path
      Mirror serv -> return (URL serv,path)
      DlFpo -> return (URL "https://dl.fedoraproject.org/pub" +//+ top, path)
