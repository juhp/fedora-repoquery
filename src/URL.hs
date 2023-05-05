{-# LANGUAGE CPP #-}

module URL (
  (+//+),
  URL(..),
  renderUrl,
  removeSubpath
) where

import Data.List.Extra (dropSuffix)

#if MIN_VERSION_http_directory(0,1,9)
import Network.HTTP.Directory((+/+))
#endif

newtype URL = URL String
  deriving Eq

renderUrl :: URL -> String
renderUrl (URL url) = url

foldSlash :: [String] -> String
foldSlash = foldr (+/+) ""

infixr 5 +//+
(+//+) :: URL -> [String] -> URL
(URL base) +//+ rel =
  URL $ base +/+ foldSlash rel

-- piecesToPath :: [String] -> String
-- piecesToPath =

-- unsafeURI :: T.Text -> URI
-- unsafeURI u =
--   case mkURI u of
--     Nothing -> error $ "failed to parse url: " ++ show u
--     Just uri -> uri

#if !MIN_VERSION_http_directory(0,1,9)
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
#endif

removeSubpath :: [String] -> String -> String
removeSubpath path url =
  let url' = (if last url == '/' then init else id) url
  in dropSuffix (foldSlash path) url'
