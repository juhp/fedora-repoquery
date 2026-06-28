import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Control.Monad (unless)
import SimpleCmd

fdrq :: ([String],Maybe String) -> IO ()
fdrq (args,mpkg) = do
  putStrLn ""
  putStrLn $ "# " ++ unwords args +-+ fromMaybe "" mpkg
  let debug = False
  (ok, out, err) <- cmdFull "fedora-repoquery" (["-d" | debug] ++ args ++ maybeToList mpkg) ""
  if null err
    then unless (isNothing mpkg || "-T" `notElem` args) $ putStrLn "stderr empty"
    else putStrLn err
  if null out
    then unless (isNothing mpkg) $ error' "no output"
    else putStrLn out
  unless ok $ error' "failed"

-- copied from dl-fedora
branched :: Int
branched = 44
current, previous, prevprev, rawhide :: String
current = show branched
previous = show (branched - 1)
prevprev = show (branched - 2)
rawhide = show (branched + 1)

tests :: [([String],Maybe String)]
tests =
  [(["rawhide"], Nothing)
  ,(["-D","rawhide"], Nothing)
  ,([current], Nothing)
  ,([previous], Nothing)
  ,(["eln"], Nothing)
  ,(["epel10"], Nothing)
  ,(["epel10.2"], Nothing)
  ,(["epel9"], Nothing)
  ,(["epel8"], Nothing)
  ,(["epel7"], Nothing)
  ,(["epel6"], Nothing)
  ,(["c10s"], Nothing)
  ,(["c9s"], Nothing)
  ,(["c8s"], Nothing)
  ,(["centos8"], Nothing)
  ,(["centos7"], Nothing)
  ,(["centos6"], Nothing)
  ,(["--cf", current], Nothing)
  ,(["-K", "epel10"], Nothing)
  ,(["-T", "rawhide"], Just "coreutils")
  ,(["--dynamic", rawhide], Just "coreutils")
  ,(["-T", current], Just "gtk4")
  ,([previous], Just "bash")
  ,([prevprev], Just "fontconfig")
  ,(["-t", previous], Just "podman")
  ,(["eln"], Just "ibus")
  ,(["epel6"], Just "ghc")
  ,(["epel7"], Just "ghc")
  ,(["epel8"], Just "ghc")
  ,(["epel9"], Just "ghc")
  ,(["epel10"], Just "ghc")
  ,(["epel10.2"], Just "ghc-srpm-macros")
  ,(["epel10.1"], Just "ghc-srpm-macros")
  ,(["c10"], Just "bash")
  ,(["c9"], Just "kernel")
  ,(["c8"], Just "bash")
  ,(["c7"], Just "gcc")
  ,(["c6"], Just "gcc")
  ,(["rawhide", "--whatprovides"], Just "perl(Regexp::Pattern::License)")
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
