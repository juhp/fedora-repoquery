{-# LANGUAGE CPP #-}

module Common (
  Manager,
  warning,
#if !MIN_VERSION_http_directory(0,1,6)
  noTrailingSlash
#endif
  ) where

import Network.HTTP.Client (Manager)
#if !MIN_VERSION_http_directory(0,1,6)
import qualified Data.Text as T
#endif

#if MIN_VERSION_simple_cmd(0,2,0)
import SimpleCmd (warning)
#else
import System.IO (hPutStrLn, stderr)

warning :: String -> IO ()
warning = hPutStrLn stderr
#endif

#if !MIN_VERSION_http_directory(0,1,6)
noTrailingSlash :: T.Text -> T.Text
noTrailingSlash = T.dropWhileEnd (== '/')
#endif
