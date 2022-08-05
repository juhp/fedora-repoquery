import Control.Monad (unless)
import SimpleCmd

fdrq :: [String] -> IO ()
fdrq args = do
  putStrLn ""
  putStrLn $ "# " ++ head args
  let debug = ["-d" | False]
  (ok, out, err) <- cmdFull "fedora-repoquery" (debug ++ args) ""
  if null err
    then if null out
         then error' "no output"
         else unless (length args == 1) $ error' "stderr empty"
    else putStrLn err
  putStrLn out
  unless ok $ error' "failed"

tests :: [[String]]
tests =
  [["rawhide", "coreutils"]
  ,["36", "fontconfig"]
  ,["eln", "ibus"]
  ,["epel9", "ghc"]
  ,["c9", "kernel"]
  ,["c8", "pandoc"]
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
