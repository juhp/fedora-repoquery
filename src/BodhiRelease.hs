{-# LANGUAGE OverloadedStrings #-}

module BodhiRelease (
  BodhiRelease (..),
  activeFedoraRelease,
  activeEPELRelease,
  activeEPELMinorRelease,
  activeBodhiFedoraReleases,
  activeBodhiEPELReleases,
  activeBodhiReleases,
  rawhideFedoraRelease
  )
where

import Data.Aeson (Object)
import Data.Function (on)
import Data.List.Extra
import Data.Maybe (mapMaybe)
import Data.Version.Extra (readVersion, showVersion, Version(..))
import Distribution.Fedora.BodhiReleases (getBodhiFedoraReleases,
                                          getBodhiEPELReleases,
                                          getBodhiReleases,
                                          lookupKey)
import Safe (headMay)
import SimpleCmd (error', (+-+))

import Types (Release, Natural, elnVersion, readRelease)

data BodhiRelease =
  BodhiRelease {releaseVersion :: String, -- to handle "eln"
                releaseState ::  String,
                releaseBranch :: Release,
                releaseComposed :: Bool,
                releasePostBeta :: Bool}
  deriving Eq

activeFedoraRelease :: Natural -> IO (Maybe BodhiRelease)
activeFedoraRelease n = do
  -- FIXME include eol releases?
  active <- activeBodhiFedoraReleases
  case sortOn releaseBranch active of
    [] -> error' "failed to find active releases with Bodhi API"
    (oldest:_) ->
      return $
      case find (\r -> releaseVersion r == show n) active of
        Just rel -> Just rel
        Nothing ->
          let oldver = read $ releaseVersion oldest in
            if n < oldver
            then Nothing
            else error' $ "unknown fedora release:" +-+ show n

activeEPELRelease :: Natural -> IO (Maybe BodhiRelease)
activeEPELRelease n = do
  active <- activeBodhiEPELReleases
  case sortOn (readVersion . releaseVersion) active of
    [] -> error' "failed to find active releases with Bodhi API"
    (oldest:_) ->
      return $
      case find (\r -> majorVer (releaseVersion r) == show n) active of
        Just rel -> Just rel
        Nothing ->
          let oldver = read . majorVer $ releaseVersion oldest in
            if n < oldver
            then Nothing
            else error' $ "unknown epel release:" +-+ show n
  where
    majorVer = takeWhile (/= '.')

activeEPELMinorRelease :: Version -> IO (Maybe BodhiRelease)
activeEPELMinorRelease ver = do
  active <- activeBodhiEPELReleases
  case find (\r -> readVersion (releaseVersion r) == ver) active of
    Just rel -> return $ Just rel
    Nothing -> do
      let minors = filter (\r -> ((==) `on` majorVer) ver (readVersion . releaseVersion $ r)) active
      if majorVer ver < fromIntegral elnVersion &&
         all (\r -> readVersion (releaseVersion r) > ver) minors
      then return Nothing
      else error' $ "unknown epel release:" +-+ showVersion ver
  where
    majorVer v =
      case headMay $ versionBranch v of
        Just m -> m
        Nothing -> error $ "no major version:" +-+ showVersion ver

activeBodhiReleases :: IO [BodhiRelease]
activeBodhiReleases =
  nubOrdOn releaseBranch . mapMaybe maybeRelease <$> getBodhiReleases

activeBodhiFedoraReleases :: IO [BodhiRelease]
activeBodhiFedoraReleases =
  nubOrdOn releaseBranch . mapMaybe maybeRelease <$> getBodhiFedoraReleases

activeBodhiEPELReleases :: IO [BodhiRelease]
activeBodhiEPELReleases =
  nubOrdOn releaseBranch . mapMaybe maybeRelease <$> getBodhiEPELReleases

maybeRelease :: Object -> Maybe BodhiRelease
maybeRelease obj = do
  version <- lookupKey "version" obj
  branch <- lookupKey "branch" obj >>= readRelease
  state <- lookupKey "state" obj
  composed <- lookupKey "composed_by_bodhi" obj
  let setting = lookupKey "setting_status" obj
  return $
    BodhiRelease version state branch composed $
    setting == Just ("post_beta" :: String)


rawhideFedoraRelease :: IO Natural
rawhideFedoraRelease = do
  actives <- activeBodhiFedoraReleases
  let pending = map releaseVersion (filter (\r -> releaseState r == "pending") actives)
  return $ read $ maximum (delete "eln" pending)
