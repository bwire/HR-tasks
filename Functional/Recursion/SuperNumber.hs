-- Super Digit

import Data.Char(digitToInt)

superDigit :: String -> String -> String
superDigit n k = sum' $ show ((read $ sum' n) * (read k)) where
  sum' n | n `elem` ["1","2","3","4","5","6","7","8","9","0"] = n
         | otherwise = sum' . show . foldl (\acc token -> acc + digitToInt token) 0 $ n
         
main :: IO ()
main = do
  line <- getLine
  let [n, k] = words line
  putStrLn (superDigit n k)         
