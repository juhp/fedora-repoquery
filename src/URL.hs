{-# LANGUAGE CPP #-}

module URL (
  (+//+),
  URL(..),
  renderUrl
) where

#if MIN_VERSION_http_directory(0,1,9)
import Network.HTTP.Directory((+/+))
#endif

newtype URL = URL String
  deriving Eq

renderUrl :: URL -> String
renderUrl (URL url) = url

infixr 5 +//+
(+//+) :: URL -> [String] -> URL
(URL base) +//+ rel =
  URL $ base +/+ foldr (+/+) "" rel

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
