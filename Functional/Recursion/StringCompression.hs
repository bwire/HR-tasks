-- String Compression 

import Data.List (group)
import Control.Monad (liftM)

compress :: String -> String
compress s = 
  let
    fl s = if l == 1 then "" else show l where l = length s
    pairs = map (\g -> ([head g], fl g)) $ group s
    codes = map (\(f, s) -> f ++ s) pairs
  in concat codes
  
main :: IO ()
main = getLine >>= putStrLn . compress
