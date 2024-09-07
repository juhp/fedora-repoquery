{-# LANGUAGE OverloadedStrings #-}

module BodhiRelease (
  BodhiRelease (..),
  activeFedoraRelease,
  activeFedoraReleases,
  fedoraReleaseState,
  rawhideFedoraRelease,
  fedoraReleasePostBeta
  )
where

import Control.Monad.Extra (when)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Fedora.Bodhi (bodhiReleases, lookupKey, makeKey)
import SimpleCmd (error')
import System.Cached.JSON (getCachedJSONQuery)

import Types (Natural)

data BodhiRelease =
  Release {releaseVersion :: String, -- to handle eln
           releaseState ::  String,
           releaseBranch :: String,
           releasePostBeta :: Bool}
  deriving Eq

-- Left is oldest active version
activeFedoraRelease :: Natural -> IO (Either Natural BodhiRelease)
activeFedoraRelease n = do
  active <- activeFedoraReleases
  when (null active) $ error' "failed to find active releases with Bodhi API"
  case L.find (\r -> releaseVersion r == show n) active of
    Just rel -> return $ Right rel
    Nothing ->
      let ordered = L.sort $ map releaseVersion active
      in return $ Left $ read $ head ordered

activeFedoraReleases :: IO [BodhiRelease]
activeFedoraReleases =
  L.nub . mapMaybe maybeRelease <$> getCachedJSONQuery "fedora-repoquery" "fedora-bodhi-releases-active" (bodhiReleases (makeKey "exclude_archived" "1")) 1000
  where
    maybeRelease obj = do
      version <- lookupKey "version" obj
      state <- lookupKey "state" obj
      branch <- lookupKey "branch" obj
      let setting = lookupKey "setting_status" obj
      return $
        Release version state branch $
        setting == Just ("post_beta" :: String)

fedoraReleaseState :: Natural -> IO String
fedoraReleaseState n = do
  eactive <- activeFedoraRelease n
  return $
    case eactive of
      Left _ -> error' $ "could find F" ++ show n ++ " release (state)"
      Right rel -> releaseState rel

rawhideFedoraRelease :: IO Natural
rawhideFedoraRelease = do
  actives <- activeFedoraReleases
  let pending = map releaseVersion (filter (\r -> releaseState r == "pending") actives)
  return $ read $ maximum (L.delete "eln" pending)

fedoraReleasePostBeta :: Natural -> IO Bool
fedoraReleasePostBeta n = do
  eactive <- activeFedoraRelease n
  return $
    case eactive of
      Left _ -> error' $ "could find F" ++ show n ++ " release (setting)"
      Right rel -> releasePostBeta rel
