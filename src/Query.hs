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

showReleaseCmd :: Bool -> RepoSource -> Release  -> Arch -> IO ()
showReleaseCmd debug reposource release arch =
  void $ showRelease debug Normal False reposource release arch

repoqueryCmd :: Bool -> Verbosity -> Release -> RepoSource -> Arch -> [String]
             -> IO ()
repoqueryCmd debug verbose release reposource arch args = do
  repoConfigs <- showRelease debug verbose True reposource release arch
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
    Fedora _ | repo == "updates" -> '-':repo
    _ -> ""

repoConfigArgs :: RepoSource -> Arch -> Release
               -> (String,URL) -> (String,(URL,[String]))
-- non-koji
repoConfigArgs (RepoSource False _chan mirror) arch release (repo,url) =
  -- FIXME default to system arch
  let archsuffix = if arch == X86_64 then "" else "-" ++ showArch arch
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
          EPEL _n -> ["Everything", showArch arch]
          EPELNext _n -> ["Everything", showArch arch]
          Fedora _ -> ["Everything", showArch arch] ++ (if arch == Source then ["tree"] else ["os" | repo /= "updates"])
          Rawhide -> ["Everything", showArch arch, if arch == Source then "tree" else "os"]
  in (reponame, (url, path))
-- koji
repoConfigArgs (RepoSource True _chan _mirror) arch release (repo,url) =
  let (compose,path) =
        case release of
          Rawhide -> (["repos", show release, "latest"],"")
          _ -> (["repos", show release ++ "-build/latest"],"")
      reponame = repo ++ "-" ++ show release ++ "-build" ++
                 if arch == X86_64 then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, ""]))
-- repoConfigArgs url (RepoCentosStream chan) arch release repo =
--   let (compose,path) = (["composes", channel chan, "latest-CentOS-Stream", "compose"], repo)
--       reponame = repo ++ "-Centos-" ++ show release ++ "-Stream" ++ "-" ++ show chan ++ if arch == X86_64 then "" else "-" ++ showArch arch
--   in (reponame, (url +//+ compose, [path, showArch arch, "os/"]))

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath", name ++ "," ++ renderUrl url, "--repo", name]

showRelease :: Bool -> Verbosity -> Bool -> RepoSource -> Release -> Arch
            -> IO [(String, URL)]
showRelease debug verbose warn reposource@(RepoSource koji _chan _mirror) release arch = do
  mgr <- httpManager
  (url,path) <- getURL debug mgr reposource release
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
          else [("releases", urlpath),
                ("updates", url +//+ ["updates",show n])]
      EPEL _n -> return [("epel",urlpath)]
      EPELNext _n -> return [("epelnext",urlpath)]
  forM repos $ \repourl -> do
    let (reponame,(url',path')) = repoConfigArgs reposource arch release repourl
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
                                if "updates" `L.isSuffixOf` reponame
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

fedoraTop :: [String]
fedoraTop = ["fedora", "linux"]

epelTop :: [String]
epelTop = ["epel"]

getURL :: Bool -> Manager -> RepoSource -> Release -> IO (URL,[String])
getURL debug mgr reposource@(RepoSource koji chan _mirror) release =
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
    EPEL n -> getFedoraServer debug mgr reposource epelTop [show n]
    EPELNext n -> getFedoraServer debug mgr reposource (epelTop ++ ["next"]) [show n]
    -- FIXME hardcoded
    Fedora n | n < 36 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive/fedora/linux", ["releases", show n])
    Fedora n -> do
      pending <- pendingFedoraRelease n
      getFedoraServer debug mgr reposource fedoraTop
        [if pending then "development" else "releases", show n]
    Rawhide -> getFedoraServer debug mgr reposource fedoraTop ["development", "rawhide"]

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
