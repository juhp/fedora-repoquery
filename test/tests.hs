import Control.Monad (unless)
import SimpleCmd

fdrq :: [String] -> IO ()
fdrq args = do
  putStrLn ""
  putStrLn $ "# " ++ unwords args
  let debug = False
  (ok, out, err) <- cmdFull "fdrq" (["-d" | debug] ++ args) ""
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
  ,["38", "fontconfig"]
  ,["-t", "37", "podman"]
  ,["eln", "ibus"]
  ,["epel9", "ghc"]
  ,["c9", "kernel"]
  ,["c8", "pandoc"]
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
