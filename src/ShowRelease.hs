{-# LANGUAGE OverloadedStrings #-}

module ShowRelease (
  showReleaseCmd,
  showRelease,
  activeFedoraReleases,
  BodhiRelease(..),
  downloadServer
  )
where

import Control.Monad.Extra (forM_, unless, void, when, whenJust)
import qualified Data.ByteString.Char8 as B
import Data.List.Extra
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.HTTP.Directory (httpLastModified, httpManager,
                               httpRedirect, Manager)
import SimpleCmd (error', (+-+))
import Text.Regex (mkRegex, subRegex)

import Arch
import BodhiRelease
import Common (warning)
import Types
import URL

showReleaseCmd :: Bool -> Bool -> RepoSource -> Release  -> Arch -> Maybe Arch
               -> Bool -> IO ()
showReleaseCmd debug redirect reposource release sysarch march testing =
  void $ showRelease debug redirect False True reposource release sysarch march testing

showRelease :: Bool -> Bool -> Bool -> Bool -> RepoSource
            -> Release -> Arch -> Maybe Arch -> Bool -> IO [(String, URL)]
showRelease debug redirect warn checkdate reposource@(RepoSource koji _chan _mirror) release sysarch march testing = do
  mgr <- httpManager
  let arch = fromMaybe sysarch march
  (url,path) <- getURL debug redirect mgr reposource release arch
  let urlpath = url +//+ path
  when debug $ print $ renderUrl urlpath
  (basicrepos,morerepos) <-
    case release of
      -- RepoKoji -> ["koji-fedora"]
      Centos n -> return ([("BaseOS",urlpath)],
                          [("AppStream",url),(if n >= 9 then "CRB" else "PowerTools",url)])
      ELN -> return ([("BaseOS",urlpath)],
                     [("AppStream",urlpath),("CRB",urlpath)])
      Rawhide -> return ([("development", urlpath)],[])
      Fedora n -> do
        pending <- pendingFedoraRelease n
        return $
          if pending
          then ([("development", urlpath)],[])
          else (("releases", urlpath) :
                ("updates", url +//+ ["updates",show n]) :
                [("updates-testing", url +//+ ["updates","testing",show n]) | testing],
                [])
      EPEL n -> return
                (("epel",urlpath) :
                 [("epel-testing",url +//+ ["testing", show n]) | testing],
                 [])
      EPELNext _n -> return ([("epelnext",urlpath)],[])
      System -> error' "showRelease: system unsupported"
  let basicrepourls =
        map (repoConfigArgs reposource sysarch march release) basicrepos
      morerepourls =
        map (repoConfigArgs reposource sysarch march release) morerepos
  forM_ basicrepourls $ \(reponame,(url',path')) -> do
    let baserepo = url' +//+ path'
    when debug $ print $ renderUrl baserepo
    unless (not checkdate || release == System) $ do
      let composeinfo =
            if koji
            then url' +//+ ["repo.json"]
            else
              case release of
                Centos 10 -> url' +//+ ["metadata","composeinfo.json"]
                Centos _ -> url' +//+ ["COMPOSE_ID"] -- ["metadata","composeinfo.json"]
                ELN -> url' +//+ ["metadata","composeinfo.json"]
                EPEL _ -> url' +//+ ["Everything", "state"]
                EPELNext _ -> url' +//+ ["Everything", "state"]
                Fedora _ -> url' +//+
                            if "updates" `isSuffixOf` reponame ||
                               "updates-testing" `isSuffixOf` reponame
                            then ["Everything", "state"]
                            else ["COMPOSE_ID"]
                Rawhide -> url' +//+ ["COMPOSE_ID"]
                System -> error' "system not supported"
      let composeUrl = renderUrl composeinfo
      when debug $ print composeUrl
      mtime <- httpLastModified mgr composeUrl
      whenJust mtime $ \utc -> do
        date <- utcToLocalZonedTime utc
        (if warn then warning else putStrLn) $ show date +-+ "<" ++ renderUrl url' ++ ">"
  return $ map (fmap (uncurry (+//+))) $ basicrepourls ++ morerepourls

getURL :: Bool -> Bool -> Manager -> RepoSource -> Release -> Arch
       -> IO (URL,[String])
getURL debug redirect mgr reposource@(RepoSource koji chan _mirror) release arch =
  case release of
    Centos n ->
      case n of
        10 ->
          let url = URL $
                if koji
                then "https://odcs.stream.centos.org/stream-10"
                else "https://composes.stream.centos.org/stream-10/production/latest-CentOS-Stream/compose/"
          in return (url,[])
        9 ->
          let url = URL $
                if koji
                then "https://odcs.stream.centos.org"
                else "https://mirror.stream.centos.org/9-stream/"
          in return (url,[])
        8 -> return (URL "http://mirror.centos.org/centos/8-stream/", [])
        _ -> error' "old Centos is not supported yet"
    ELN ->
      return (URL "https://odcs.fedoraproject.org/composes", [channel chan, "latest-Fedora-ELN", "compose"])
    EPEL n | n < 7 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive/epel", [show n])
    EPEL n -> getFedoraServer debug redirect mgr reposource ["epel"] [show n]
    EPELNext n -> getFedoraServer debug redirect mgr reposource ["epel","next"] [show n]
    Fedora n -> do
      ebodhirelease <- activeFedoraRelease n
      case ebodhirelease of
        Left oldest ->
          if n < oldest
          then
          return
          (URL "https://archives.fedoraproject.org/pub/archive" +//+ fedoraTop, ["releases", show n])
          else error' $ "unknown fedora release:" +-+ show n
        Right rel ->
          let pending = releaseState rel == "pending"
              rawhide = pending && releaseBranch rel == "rawhide"
              releasestr = if rawhide then "rawhide" else show n
          in getFedoraServer debug redirect mgr reposource fedoraTop
             [if pending then "development" else "releases", releasestr]
    Rawhide -> getFedoraServer debug redirect mgr reposource fedoraTop ["development", "rawhide"]
    System -> error' "getURL: system unsupported"
  where
    fedoraTop =
      -- FIXME support older archs
      if arch `elem` [PPC64LE, S390X]
      then ["fedora-secondary"]
      else ["fedora", "linux"]

repoConfigArgs :: RepoSource -> Arch -> Maybe Arch -> Release -> (String,URL)
               -> (String,(URL,[String]))
-- non-koji
repoConfigArgs (RepoSource False _chan mirror) sysarch march release (repo,url) =
  let arch = fromMaybe sysarch march
      archsuffix = if arch == sysarch then "" else "-" ++ showArch arch
      reponame = repoVersion ++ archsuffix ++
                 case mirror of
                   DownloadFpo -> ""
                   Mirror serv ->
                     '-' : takeWhile (/= '/') (subRegex (mkRegex "https?://") serv "")
                   DlFpo -> "-dl.fpo"
      path =
        case release of
          Centos _ -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          ELN -> [repo, showArch arch] ++ (if arch == Source then ["tree"] else ["os"])
          EPEL n -> (if n >= 8 then ("Everything" :) else id) [showArch arch] ++ ["tree" | arch == Source]
          EPELNext _n -> ["Everything", showArch arch]
          Fedora _ -> ["Everything", showArch arch] ++ (if arch == Source then ["tree"] else ["os" | repo `elem` ["releases","development"]])
          Rawhide -> ["Everything", showArch arch, if arch == Source then "tree" else "os"]
          System -> error' "repoConfigArgs: system unsupported"
  in (reponame, (url, path ++ ["/"]))
  where
    repoVersion :: String
    repoVersion =
      if release == Rawhide
      then "fedora-rawhide"
      else
        show release ++
        case release of
          Centos _ -> '-':repo
          ELN -> '-':repo
          EPEL _ -> if repo == "epel-testing" then "-testing" else ""
          Fedora _ | repo /= "releases" -> '-':repo
          _ -> ""
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

getFedoraServer :: Bool -> Bool -> Manager -> RepoSource -> [String] -> [String]
                -> IO (URL,[String])
getFedoraServer debug redirect mgr (RepoSource koji _ mirror) top path =
  if koji
  then return (URL "https://kojipkgs.fedoraproject.org",[])
  else
    case mirror of
      DownloadFpo -> do
        let url = URL downloadServer +//+ top ++ path
            rurl = renderUrl url
        redir <-
          if redirect
          then fmap B.unpack <$> httpRedirect mgr rurl
          else return Nothing
        case redir of
          Nothing -> do
            when redirect $
              warning $ "no redirect for" +-+ rurl
            return (URL downloadServer +//+ top,path)
          Just actual -> do
            when debug $ do
              warning rurl
              warning $ "redirected to" +-+ show actual
            let actualstr =
                  if "https://ftp.yzyu.jp/" `isPrefixOf` actual
                  then replace "https://" "http://" actual
                  else actual
            when (actual /= actualstr) $
              warning $ "replacing to" +-+ actualstr
            return (URL $ removeSubpath path actualstr, path)
      -- FIXME how to handle any path
      Mirror serv -> return (URL serv,path)
      DlFpo -> return (URL "https://dl.fedoraproject.org/pub" +//+ top, path)

downloadServer :: String
downloadServer = "https://download.fedoraproject.org/pub"
