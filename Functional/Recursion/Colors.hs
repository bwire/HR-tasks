-- Sequence full of colors

import Control.Monad(replicateM)

convert :: Char -> [Int]
convert c = case c of
  'R' -> [1, 0, 0, 0]
  'G' -> [0, 1, 0, 0]
  'B' -> [0, 0, 1, 0]
  'Y' -> [0, 0, 0, 1]
    
checkStep :: [Int] -> Maybe [Int] -> Maybe [Int]
checkStep elem Nothing = Nothing
checkStep elem (Just acc) =
  let [r, g, b, y] = zipWith (+) acc elem 
  in if abs (r - g) <= 1 && abs (b - y) <= 1 
     then Just [r, g, b, y]
     else Nothing

checkSeq :: String -> String
checkSeq s = case foldr checkStep (Just [0, 0, 0, 0]) (map convert s) of
  Nothing -> "False"
  Just [r,g,b,y] -> if r == g && b == y then "True" else "False"

main :: IO ()
main = do
  n <- getLine
  cs <- replicateM (read n) getLine
  mapM_ putStrLn (map checkSeq cs)