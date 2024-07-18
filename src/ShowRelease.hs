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
import Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra
import Data.Maybe (fromMaybe)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.Curl (curlHead, CurlOption(..))
import SimpleCmd (error', (+-+))
import Text.Regex (mkRegex, subRegex)

import Arch
import BodhiRelease
import Common (warning)
import Types
import URL

showReleaseCmd :: Bool -> Bool -> RepoSource -> Release  -> Arch -> Maybe Arch
               -> Bool -> IO ()
showReleaseCmd debug dynredir reposource release sysarch march testing =
  void $ showRelease debug dynredir False True reposource release sysarch march testing

showRelease :: Bool -> Bool -> Bool -> Bool -> RepoSource
            -> Release -> Arch -> Maybe Arch -> Bool -> IO [(String, URL)]
showRelease debug dynredir warn checkdate reposource@(RepoSource koji _chan _mirror) release sysarch march testing = do
  let arch = fromMaybe sysarch march
  (url,path) <- getURL debug dynredir reposource release arch
  let urlpath = url +//+ path
  when debug $ print $ renderUrl Dir urlpath
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
  rawhide <- rawhideFedoraRelease
  let basicrepourls =
        map (repoConfigArgs reposource sysarch march rawhide release) basicrepos
      morerepourls =
        map (repoConfigArgs reposource sysarch march rawhide release) morerepos
  forM_ basicrepourls $ \(reponame,(url',path')) -> do
    let baserepo = url' +//+ path'
    when debug $ print $ renderUrl Dir baserepo
    unless (not checkdate || release == System) $ do
      let composeinfo =
            url' +//+
            if koji
            then ["repo.json"]
            else
              case release of
                Centos 10 -> ["metadata","composeinfo.json"]
                Centos _ -> ["COMPOSE_ID"] -- ["metadata","composeinfo.json"]
                ELN -> ["metadata","composeinfo.json"]
                EPEL _ -> ["Everything", "state"]
                EPELNext _ -> ["Everything", "state"]
                Fedora _ -> if "updates" `isInfixOf` reponame ||
                               "updates-testing" `isInfixOf` reponame
                            then ["Everything", "state"]
                            else ["COMPOSE_ID"]
                Rawhide -> ["COMPOSE_ID"]
                System -> error' "system not supported"
      let composeUrl = renderUrl File composeinfo
      when debug $ print composeUrl
      mtimestr <- curlGetHeader "Last-Modified" composeUrl
      whenJust mtimestr $ \timestr -> do
        utc <- parseTimeM False defaultTimeLocale rfc822DateFormat timestr
        date <- utcToLocalZonedTime utc
        (if warn then warning else putStrLn) $ show date +-+ "<" ++ renderUrl Dir url' ++ ">"
  return $ map (fmap (uncurry (+//+))) $ basicrepourls ++ morerepourls

getURL :: Bool -> Bool -> RepoSource -> Release -> Arch
       -> IO (URL,[String])
getURL debug dynredir reposource@(RepoSource koji chan _mirror) release arch =
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
        8 -> return (URL "http://vault.centos.org/8-stream/", [])
        _ -> error' "old Centos is not supported"
    ELN ->
      return (URL "https://odcs.fedoraproject.org/composes", [channel chan, "latest-Fedora-ELN", "compose"])
    EPEL n | n < 7 ->
               return
               (URL "https://archives.fedoraproject.org/pub/archive/epel", [show n])
    EPEL n -> getFedoraServer debug dynredir reposource ["epel"] [show n]
    EPELNext n -> getFedoraServer debug dynredir reposource ["epel","next"] [show n]
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
          in getFedoraServer debug dynredir reposource fedoraTop
             [if pending then "development" else "releases", releasestr]
    Rawhide -> getFedoraServer debug dynredir reposource fedoraTop ["development", "rawhide"]
    System -> error' "getURL: system unsupported"
  where
    fedoraTop =
      -- FIXME support older archs
      if arch `elem` [PPC64LE, S390X]
      then ["fedora-secondary"]
      else ["fedora", "linux"]

repoConfigArgs :: RepoSource -> Arch -> Maybe Arch -> Natural -> Release
               -> (String,URL) -> (String,(URL,[String]))
-- non-koji
repoConfigArgs (RepoSource False _chan mirror) sysarch march rawhide release (repo,url) =
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
      if release `elem` [Rawhide, Fedora rawhide]
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
repoConfigArgs (RepoSource True _chan _mirror) sysarch march _rawhide release (repo,url) =
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

getFedoraServer :: Bool -> Bool -> RepoSource -> [String] -> [String]
                -> IO (URL,[String])
getFedoraServer debug dynredir (RepoSource koji _ mirror) top path =
  if koji
  then return (URL "https://kojipkgs.fedoraproject.org",[])
  else
    case mirror of
      DownloadFpo -> do
        let url = URL downloadServer +//+ top ++ path
            rurl = renderUrl Dir url
        when debug $ print rurl
        redir <-
          if dynredir
          then return Nothing
          else curlGetHeader "location" rurl
        when debug $ print redir
        case redir of
          Nothing -> do
            unless dynredir $
              warning $ "slow redirection for" +-+ rurl
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

-- FIXME check CurlCode(CurlOK) status
curlGetHeader :: CI.CI String -> String -> IO (Maybe String)
curlGetHeader field url = do
  (_status,headers) <- curlHead url [CurlConnectTimeoutMS 5000]
  -- tail because curl leaves space before field value
  return $ tail <$> lookup field (map (first CI.mk) headers)
