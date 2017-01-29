-- Prefix Compression

import Control.Monad(replicateM)

getPref :: String -> String -> String
getPref [] _ = []
getPref _ [] = []
getPref (x1:xs1) (x2:xs2) | x1 == x2 = x1 : getPref xs1 xs2
                          | otherwise = []
                          
main :: IO ()
main = do
  [s1, s2] <- replicateM 2 getLine
  let pref = getPref s1 s2
  let len = length pref
  putStrLn $ (show len) ++ " " ++ pref 
  putStrLn $ (show $ length s1 - len) ++ " " ++ drop len s1 
  putStrLn $ (show $ length s2 - len) ++ " " ++ drop len s2