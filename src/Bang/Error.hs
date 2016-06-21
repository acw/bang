module Bang.Error(
         exit
       )
 where

import System.Exit(ExitCode(..), exitWith)

exit :: String -> IO b
exit x =
  do putStrLn ("ERROR: " ++ x)
     exitWith (ExitFailure 1)
