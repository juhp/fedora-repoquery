import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Control.Monad (unless)
import SimpleCmd

fdrq :: ([String],Maybe String) -> IO ()
fdrq (args,mpkg) = do
  putStrLn ""
  putStrLn $ "# " ++ unwords args +-+ fromMaybe "" mpkg
  let debug = False
  (ok, out, err) <- cmdFull "fdrq" (["-d" | debug] ++ args ++ maybeToList mpkg) ""
  if null err
    then unless (isNothing mpkg) $ putStrLn "stderr empty"
    else putStrLn err
  if null out
    then unless (isNothing mpkg) $ error' "no output"
    else putStrLn out
  unless ok $ error' "failed"

-- copied from dl-fedora
branched :: Int
branched = 40
current, previous, prevprev, rawhide :: String
current = show branched
previous = show (branched - 1)
prevprev = show (branched - 2)
rawhide = show (branched + 1)

tests :: [([String],Maybe String)]
tests =
  [(["rawhide"], Nothing)
  ,([current], Nothing)
  ,([previous], Nothing)
  ,([prevprev], Nothing)
  ,(["rawhide"], Just "coreutils")
  ,([rawhide], Just "coreutils")
  ,([current], Just "gtk4")
  ,([previous], Just "bash")
  ,([prevprev], Just "fontconfig")
  ,(["-t", previous], Just "podman")
  ,(["eln"], Just "ibus")
  ,(["epel9"], Just "ghc")
  ,(["c10"], Just "bash")
  ,(["c9"], Just "kernel")
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
