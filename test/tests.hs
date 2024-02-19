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

tests :: [([String],Maybe String)]
tests =
  [(["rawhide"], Nothing)
  ,(["40"], Nothing)
  ,(["39"], Nothing)
  ,(["38"], Nothing)
  ,(["rawhide"], Just "coreutils")
  ,(["40"], Just "gtk4")
  ,(["39"], Just "bash")
  ,(["38"], Just "fontconfig")
  ,(["-t", "39"], Just "podman")
  ,(["eln"], Just "ibus")
  ,(["epel9"], Just "ghc")
  ,(["c10"], Just "bash")
  ,(["c9"], Just "kernel")
  ,(["c8"], Just "pandoc")
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
