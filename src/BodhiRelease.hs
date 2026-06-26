{-# LANGUAGE OverloadedStrings #-}

module BodhiRelease (
  BodhiRelease (..),
  activeFedoraRelease,
  activeEPELRelease,
  activeEPELMinorRelease,
  activeFedoraReleases,
  activeEPELReleases,
  rawhideFedoraRelease
  )
where

import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Version.Extra (readVersion, Version)
import Distribution.Fedora.BodhiReleases (getBodhiFedoraReleases,
                                          getBodhiEPELReleases, lookupKey)
import SimpleCmd (error')

import Types (Natural)

data BodhiRelease =
  Release {releaseVersion :: String, -- to handle "eln"
           releaseState ::  String,
           releaseBranch :: String,
           releaseComposed :: Bool,
           releasePostBeta :: Bool}
  deriving Eq

-- Left is oldest active version
activeFedoraRelease :: Natural -> IO (Either Natural BodhiRelease)
activeFedoraRelease n = do
  active <- activeFedoraReleases
  case L.sortOn releaseVersion active of
    [] -> error' "failed to find active releases with Bodhi API"
    (oldest:_) ->
      return $
      case L.find (\r -> releaseVersion r == show n) active of
        Just rel -> Right rel
        Nothing -> Left $ read $ releaseVersion oldest

-- Left is oldest active major version
activeEPELRelease :: Natural -> IO (Either Natural BodhiRelease)
activeEPELRelease n = do
  active <- activeEPELReleases
  case L.sortOn (readVersion . releaseVersion) active of
    [] -> error' "failed to find active releases with Bodhi API"
    (oldest:_) ->
      return $
      case L.find (\r -> majorVer (releaseVersion r) == show n) active of
        Just rel -> Right rel
        Nothing -> Left $ read $ majorVer $ releaseVersion oldest
  where
    majorVer = takeWhile (/= '.')

-- FIXME Either for unknown minor
activeEPELMinorRelease :: Version -> IO (Maybe BodhiRelease)
activeEPELMinorRelease ver = do
  active <- activeEPELReleases
  return $ L.find (\r -> readVersion (releaseVersion r) == ver) active

activeFedoraReleases :: IO [BodhiRelease]
activeFedoraReleases =
  L.nub . mapMaybe maybeRelease <$> getBodhiFedoraReleases
  where
    maybeRelease obj = do
      version <- lookupKey "version" obj
      branch <- lookupKey "branch" obj
      state <- lookupKey "state" obj
      composed <- lookupKey "composed_by_bodhi" obj
      let setting = lookupKey "setting_status" obj
      return $
        Release version state branch composed $
        setting == Just ("post_beta" :: String)

activeEPELReleases :: IO [BodhiRelease]
activeEPELReleases =
  L.nub . mapMaybe maybeRelease <$> getBodhiEPELReleases
  where
    maybeRelease obj = do
      version <- lookupKey "version" obj
      branch <- lookupKey "branch" obj
      state <- lookupKey "state" obj
      composed <- lookupKey "composed_by_bodhi" obj
      let setting = lookupKey "setting_status" obj
      return $
        Release version state branch composed $
        setting == Just ("post_beta" :: String)

rawhideFedoraRelease :: IO Natural
rawhideFedoraRelease = do
  actives <- activeFedoraReleases
  let pending = map releaseVersion (filter (\r -> releaseState r == "pending") actives)
  return $ read $ maximum (L.delete "eln" pending)
