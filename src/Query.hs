{-# LANGUAGE CPP, OverloadedStrings #-}

module Query (
  getServer,
  getFedoraServer,
  repoqueryCmd,
  showReleaseCmd,
  downloadServer
  ) where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.Maybe
import qualified Data.List as L
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
      queryformat = "%{repoid}: %{name}-%{version}-%{release}.%{arch}"
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
    putStrLn $ L.intercalate "\n" (simplifyBuildroot res)
  where
    simplifyBuildroot :: [String] -> [String]
    simplifyBuildroot [] = []
    simplifyBuildroot [s] = [s]
    simplifyBuildroot ls =
      let (bs,rs) = partitionEithers $ map eitherRepo ls
      in rs ++ deleteRepos rs bs

    eitherRepo :: String -> Either String String
    eitherRepo s =
      if "Buildroot-" `L.isPrefixOf` s
      then Left s
      else Right s

    deleteRepos :: [String] -> [String] -> [String]
    deleteRepos _ [] = []
    deleteRepos [] bs = bs
    deleteRepos rs (b:bs) =
      let p = dropWhile (/= ' ') b
      in if not (any (p `L.isSuffixOf`) rs)
         then b : deleteRepos rs bs
         else deleteRepos rs bs

-- majorVersion :: Release -> String
-- majorVersion (Fedora n) = show n
-- majorVersion Rawhide = "rawhide"

repoConfigArgs :: URL -> RepoSource -> Arch -> Release
               -> String -> (String,(URL,[String]))
-- non-koji
repoConfigArgs url (RepoSource False _chan mirror) arch release repo =
  let archsuffix = if arch == X86_64 then "" else "-" ++ showArch arch
      reponame = repo ++ "-" ++ show release ++ archsuffix ++
                 case mirror of
                   DownloadFpo -> ""
                   Mirror serv ->
                     '-' : subRegex (mkRegex "https?://") serv ""
                   DlFpo -> "-dl.fpo"
      mpath = case release of
                EPEL _n -> Just [showArch arch,""]
                EPELNext _n -> Just [showArch arch,""]
                _ -> Nothing
  in (reponame, (url, fromMaybe [repo, showArch arch, if arch == Source then "tree" else "os", ""] mpath))
-- koji
repoConfigArgs url (RepoSource True _chan _mirror) arch release repo =
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
  path <- getReleasePath reposource release
  url <- getServer mgr reposource release path
  when debug $ putStrLn $ renderUrl url
  let repos =
        case release of
          -- RepoKoji -> ["koji-fedora"]
          Centos _ -> ["BaseOS", "AppStream", "PowerTools"]
          ELN -> ["BaseOS", "AppStream", "CRB"]
          _ -> ["Everything"]
      repoConfigs = map (repoConfigArgs url reposource arch release) repos
      (url',path') = snd (head repoConfigs)
      baserepo = url' +//+ path'
  when debug $ putStrLn $ renderUrl baserepo
  ok <- httpExists mgr (renderUrl baserepo)
  if ok
    then do
    unless (verbose == Quiet) $
      forM_ (L.nub (map (fst . snd) repoConfigs)) $ \ topurl -> do
      mtime <- do
        let composeinfo =
              topurl +//+ if koji
                          then ["repo.json"]
                          else
                            case release of
                              Centos _ -> ["COMPOSE_ID"] -- ["metadata","composeinfo.json"]
                              Rawhide -> ["COMPOSE_ID"]
                              Fedora _ -> ["COMPOSE_ID"]
                              EPEL _ -> ["state"]
                              EPELNext _ -> ["state"]
                              ELN -> ["metadata","composeinfo.json"]
                              -- Centos _ -> ["state?"]
        exists <- httpExists mgr (renderUrl composeinfo)
        if exists
          then httpLastModified mgr (renderUrl composeinfo)
          else return Nothing
      whenJust mtime $ \utc -> do
        date <- utcToLocalZonedTime utc
        (if warn then warning else putStrLn) $ show date ++ " <" ++ renderUrl topurl ++ ">"
    return $ map joinUrl repoConfigs
    else
      error' $ renderUrl baserepo ++ " not found"
  where
    joinUrl (n,(u,p)) = (n, u +//+ p)

downloadServer :: String
downloadServer = "https://download.fedoraproject.org/pub"

getServer :: Manager -> RepoSource -> Release -> [FilePath] -> IO URL
getServer mgr reposource@(RepoSource koji chan _mirror) release path =
  case release of
    Centos 9 ->
      return $ URL $
      if koji
      then "https://odcs.stream.centos.org"
      else "http://mirror.stream.centos.org/9-stream"
    Centos 8 -> return $ URL "http://mirror.centos.org/centos/8-stream"
    ELN -> return $ URL "https://odcs.fedoraproject.org/composes" +//+ [channel chan, "latest-Fedora-ELN", "compose"]
    _ -> getFedoraServer mgr reposource path

getFedoraServer :: Manager -> RepoSource -> [FilePath] -> IO URL
getFedoraServer mgr (RepoSource koji _ mirror) path =
  if koji
  then return $ URL "http://kojipkgs.fedoraproject.org"
  else
    case mirror of
      DownloadFpo -> do
        redir <- httpRedirect mgr (renderUrl (URL downloadServer +//+ path))
        case redir of
          Nothing -> return (URL downloadServer +//+ path)
          Just actual -> return $ URL (B.unpack actual)
      -- FIXME how to handle any path
      Mirror serv -> return $ URL serv
      DlFpo -> return $ URL "https://dl.fedoraproject.org/pub" +//+ path

-- FIXME is fedora download specific
getReleasePath :: RepoSource -> Release -> IO [String]
getReleasePath _reposource release =
  case release of
    Rawhide -> return ["fedora/linux/development/rawhide/"]
    Fedora n -> do
      pending <- mapMaybe (lookupKey "branch") <$> getCachedJSONQuery "fedora-bodhi-releases-pending" "fedora-bodhi-releases-pending" (bodhiReleases (makeKey "state" "pending")) 1000
      if show release `elem` pending
        then return ["fedora/linux/development", show n, ""]
        else return ["fedora/linux/releases", show n, ""]
    EPEL n -> return ["epel", show n, "Everything/"]
    EPELNext n -> return ["epel", "next", show n, "Everything/"]
    ELN -> return ["eln"]
    Centos _ -> return ["centos"]
