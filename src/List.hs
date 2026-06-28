module List (
  listVersionsCmd
  ) where

import Data.List.Extra
import Data.Ord (Down(Down))
import SimpleCmd ((+-+))

import BodhiRelease (activeBodhiReleases, BodhiRelease(..))

listVersionsCmd :: IO ()
listVersionsCmd =
  activeBodhiReleases >>= mapM_ printRelease . sortOn releaseSorter
  where
    printRelease rel =
      putStrLn $ releaseBranch rel +-+ releaseState rel

    releaseSorter rel =
      (Down (releaseState rel), Down(releaseVersion rel))
