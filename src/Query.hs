module Query (
  repoqueryCmd,
  showMinor
  ) where

import Control.Monad.Extra
import Data.Maybe
import Data.Either
import qualified Data.List as L
import Network.HTTP.Directory
import SimpleCmd
import System.Directory (doesFileExist)

import Distribution.Fedora.Branch

import Common
import Types

repoqueryCmd :: Verbosity -> Branch -> Manager -> String -> RepoSource
             -> Arch -> [String] -> IO ()
repoqueryCmd verbose branch mgr server reposource arch args = do
  repoConfigs <- showMinor verbose True branch mgr server reposource arch
  let qfAllowed = not $ any (`elem` ["-i","--info","-l","--list","-s","--source","--nvr","--nevra","--envra","-qf","--queryformat"]) args
      queryformat = "%{repoid}: %{name}-%{version}-%{release}.%{arch}"
  -- LANG=C.utf8
  rhsm <- doesFileExist "/etc/dnf/plugins/subscription-manager.conf"
  res <- cmdLines "dnf"
         ("repoquery" :
          ["--quiet" | verbose /= Verbose] ++
          -- https://bugzilla.redhat.com/show_bug.cgi?id=1876828
          ["--disableplugin=subscription-manager" | rhsm] ++
          (if qfAllowed then ["--qf", queryformat] else []) ++
          ["--setopt=module_platform_id=platform:" ++ show branch] ++
          concatMap renderRepoConfig repoConfigs ++
          args)
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

repoConfigArgs :: String -> RepoSource -> Arch -> Branch
               -> String -> (String,(String,String))
repoConfigArgs server (RepoFedora _) arch branch repo =
  let reponame = repo ++ "-" ++ show branch ++
                 if arch == X86_64 then "" else "-" ++ showArch arch
      (compose,mpath) =
        case branch of
          Rawhide -> ("fedora/linux/development/rawhide", Nothing)
          Fedora n ->
            if False {-n == branched-}
            then ("fedora/linux/development" +/+ show n, Nothing)
            else ("fedora/linux/releases" +/+ show n, Nothing)
          EPEL n -> ("epel" +/+ show n +/+ "Everything", Just (showArch arch ++ "/"))
  in (reponame, (server +/+ compose, fromMaybe (repo +/+ showArch arch +/+ (if arch == Source then "tree" else "os") ++ "/") mpath))
repoConfigArgs server RepoKoji arch branch repo =
  let (compose,path) =
        case branch of
          Rawhide -> ("repos" +/+ show branch +/+ "latest","")
          _ -> ("repos" +/+ show branch ++ "-build/latest","")
      reponame = repo ++ "-" ++ show branch ++ "-build" ++
                 if arch == X86_64 then "" else "-" ++ showArch arch
  in (reponame, (server +/+ compose, path +/+ showArch arch ++ "/"))
repoConfigArgs server (RepoCentosStream devel) arch branch repo =
  let (compose,path) = ("composes" +/+ (if devel then "development" else "test") +/+ "latest-CentOS-Stream/compose",repo)
      reponame = repo ++ "-Centos-" ++ show branch ++ "-Stream" ++ "-" ++ (if devel then "devel" else "test") ++ if arch == X86_64 then "" else "-" ++ showArch arch
  in (reponame, (server +/+ compose, path +/+ showArch arch +/+ "os/"))

renderRepoConfig :: (String, String) -> [String]
renderRepoConfig (name, url) =
  ["--repofrompath", name ++ "," ++ url, "--repo", name]

showMinor :: Verbosity -> Bool -> Branch -> Manager -> String
          -> RepoSource -> Arch
          -> IO [(String, String)]
showMinor verbose warn branch mgr server reposource arch = do
  let repos =
        case reposource of
          RepoFedora _ -> ["Everything"]
          RepoKoji -> ["koji-fedora"]
          RepoCentosStream _ -> ["BaseOS", "AppStream", "CRB"]
      repoConfigs = map (repoConfigArgs server reposource arch branch) repos
      (url,path) = snd (head repoConfigs)
      baserepo = url +/+ path
  ok <- httpExists mgr baserepo
  if ok
    then do
    unless (verbose == Quiet) $
      forM_ (L.nub (map (fst . snd) repoConfigs)) $ \ topurl -> do
      mdate <- do
        let composeinfo =
              topurl +/+ case reposource of
                           RepoKoji -> "repo.json"
                           RepoCentosStream _ -> "metadata/composeinfo.json"
                           RepoFedora _ ->
                             case branch of
                               Rawhide -> "COMPOSE_ID"
                               Fedora _ -> "COMPOSE_ID"
                               EPEL _ -> "state"
        exists <- httpExists mgr composeinfo
        if exists
          then fmap show <$> httpLastModified mgr composeinfo
          else return Nothing
      whenJust mdate $ \date ->
        (if warn then warning else putStrLn) $ date ++ " <" ++ topurl ++ ">"
    return $ map joinUrl repoConfigs
    else
      error' $ baserepo ++ " not found"
  where
    joinUrl (n,(u,p)) = (n, u +/+ p)
