module Cache
  (cacheSize,
   cleanEmptyCaches)
where

import Control.Monad
import Data.List.Extra
import SimpleCmd
import System.Directory
import System.FilePath

cacheDir :: IO FilePath
cacheDir = do
  dirs <- lines <$> shell "ls -d /var/tmp/dnf-*"
  case dirs of
    [] -> error' "no /var/tmp/dnf-*/ cache found"
    [d] -> return d
    _ -> error' "More than one /var/tmp/dnf-*/ cache found"

cacheSize :: IO ()
cacheSize = do
  cache <- cacheDir
  cmd_ "du" ["-sh", cache]

cleanEmptyCaches :: IO ()
cleanEmptyCaches = do
  cache <- cacheDir
  withCurrentDirectory cache $ do
    repocaches <- groupOn cachePrefix .
                  filter (\ f -> '.' `notElem` f && '-' `elem` f) <$>
                  listDirectory "."
    mapM_ removeEmptyCaches repocaches
  where
    removeEmptyCaches :: [FilePath] -> IO ()
    removeEmptyCaches [] = return ()
    removeEmptyCaches (r:rs) = do
      let repodata = r </> "repodata"
      exists <- doesDirectoryExist repodata
      if exists
        then do
        files <- listDirectory repodata
        when (null files) $ do
          putStrLn $ "removing " ++ r
          removeDirectory repodata
          removeDirectory r
        else putStrLn ("removing " ++ r) >> removeDirectory r
      removeEmptyCaches rs

-- "AppStream-8-312ff1df17be2171" -> "AppStream-8-"
cachePrefix :: FilePath -> String
cachePrefix = dropWhileEnd (/= '-')
