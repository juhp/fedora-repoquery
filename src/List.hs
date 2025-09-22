module List (
  listVersionsCmd
  ) where

import Data.List.Extra
import Data.Ord (Down(Down))
import SimpleCmd ((+-+))

import Release (activeFedoraReleases, BodhiRelease(..))

listVersionsCmd :: IO ()
listVersionsCmd =
  activeFedoraReleases >>= mapM_ printRelease . sortOn releaseSorter
  where
    printRelease (Release _version state branch _composed _postbeta) =
      putStrLn $ branch +-+ state

    releaseSorter rel =
      (Down (releaseState rel), Down(releaseVersion rel))
