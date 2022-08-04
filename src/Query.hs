{-# LANGUAGE CPP, OverloadedStrings #-}

module Query (
  getServer,
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
import Distribution.Fedora.Branch
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

showReleaseCmd :: Branch  -> RepoSource -> Arch -> IO ()
showReleaseCmd branch reposource arch =
  void $ showRelease Normal False branch reposource arch

repoqueryCmd :: Bool -> Verbosity -> Branch -> RepoSource -> Arch -> [String]
             -> IO ()
repoqueryCmd debug verbose branch reposource arch args = do
  repoConfigs <- showRelease verbose True branch reposource arch
  let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","-qf","--queryformat"]) args
      queryformat = "%{repoid}: %{name}-%{version}-%{release}.%{arch}"
  -- LANG=C.utf8
  rhsm <- doesFileExist "/etc/dnf/plugins/subscription-manager.conf"
  let cmdargs = "repoquery" :
                ["--quiet" | verbose /= Verbose] ++
                -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
                ["--disableplugin=subscription-manager" | rhsm] ++
                (if qfAllowed then ["--qf", queryformat] else []) ++
                ["--setopt=module_platform_id=platform:" ++ show branch] ++
                concatMap renderRepoConfig repoConfigs ++
                args
  when debug $
    putStrLn $ unwords $ "\ndnf" : map show cmdargs
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

-- majorVersion :: Branch -> String
-- majorVersion (Fedora n) = show n
-- majorVersion Rawhide = "rawhide"

repoConfigArgs :: URL -> RepoSource -> Arch -> Branch
               -> String -> (String,(URL,[String]))
repoConfigArgs url (RepoFedora mirror) arch branch repo =
  let archsuffix = if arch == X86_64 then "" else "-" ++ showArch arch
      reponame = repo ++ "-" ++ show branch ++ archsuffix ++
                 case mirror of
                   DownloadFpo -> ""
                   Mirror serv ->
                     '-' : subRegex (mkRegex "https?://") serv ""
                   DlFpo -> "-dl.fpo"
      mpath = case branch of
                EPEL _n -> Just [showArch arch,""]
                EPELNext _n -> Just [showArch arch,""]
                _ -> Nothing
  in (reponame, (url, fromMaybe [repo, showArch arch, if arch == Source then "tree" else "os", ""] mpath))
repoConfigArgs url RepoKoji arch branch repo =
  let (compose,path) =
        case branch of
          Rawhide -> (["repos", show branch, "latest"],"")
          _ -> (["repos", show branch ++ "-build/latest"],"")
      reponame = repo ++ "-" ++ show branch ++ "-build" ++
                 if arch == X86_64 then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, ""]))
repoConfigArgs url (RepoCentosStream devel) arch branch repo =
  let (compose,path) = (["composes", if devel then "development" else "test", "latest-CentOS-Stream", "compose"], repo)
      reponame = repo ++ "-Centos-" ++ show branch ++ "-Stream" ++ "-" ++ (if devel then "devel" else "test") ++ if arch == X86_64 then "" else "-" ++ showArch arch
  in (reponame, (url +//+ compose, [path, showArch arch, "os/"]))

renderRepoConfig :: (String, URL) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath", name ++ "," ++ renderUrl url, "--repo", name]

showRelease :: Verbosity -> Bool -> Branch -> RepoSource -> Arch
            -> IO [(String, URL)]
showRelease verbose warn branch reposource arch = do
  mgr <- httpManager
  path <- getReleasePath reposource branch
  url <- getServer mgr reposource path
  let repos =
        case reposource of
          RepoFedora _ -> ["Everything"]
          RepoKoji -> ["koji-fedora"]
          RepoCentosStream _ -> ["BaseOS", "AppStream", "CRB"]
      repoConfigs = map (repoConfigArgs url reposource arch branch) repos
      (url',path') = snd (head repoConfigs)
      baserepo = url' +//+ path'
  ok <- httpExists mgr (renderUrl baserepo)
  if ok
    then do
    unless (verbose == Quiet) $
      forM_ (L.nub (map (fst . snd) repoConfigs)) $ \ topurl -> do
      mtime <- do
        let composeinfo =
              topurl +//+ case reposource of
                           RepoKoji -> ["repo.json"]
                           RepoCentosStream _ ->
                             ["metadata","composeinfo.json"]
                           RepoFedora _ ->
                             case branch of
                               Rawhide -> ["COMPOSE_ID"]
                               Fedora _ -> ["COMPOSE_ID"]
                               EPEL _ -> ["state"]
                               EPELNext _ -> ["state"]
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

getServer :: Manager -> RepoSource -> [FilePath] -> IO URL
getServer mgr reposource path =
  case reposource of
    RepoKoji ->
      return $ URL "http://kojipkgs.fedoraproject.org"
    RepoCentosStream _ ->
      return $ URL "https://odcs.stream.rdu2.redhat.com"
    RepoFedora mirror ->
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
getReleasePath :: RepoSource -> Branch -> IO [String]
getReleasePath _reposource branch =
  case branch of
    Rawhide -> return ["fedora/linux/development/rawhide/"]
    Fedora n -> do
      pending <- mapMaybe (lookupKey "branch") <$> getCachedJSONQuery "fedora-bodhi-releases-pending" "fedora-bodhi-releases-pending" (bodhiReleases (makeKey "state" "pending")) 1000
      if show branch `elem` pending
      then return ["fedora/linux/development", show n, ""]
      else return ["fedora/linux/releases", show n, ""]
    EPEL n -> return ["epel", show n, "Everything/"]
    EPELNext n -> return ["epel", "next", show n, "Everything/"]
