-- Rotate String

import Control.Monad(replicateM, forM_)

main :: IO ()
main = do
  n <- return . read =<< getLine
  inputs <- replicateM n getLine
  forM_ inputs (putStrLn . unwords . rotateString)
 
  
rotateString :: String -> [String]
rotateString xs = [let (f, s) = splitAt n xs in s ++ f | n <- [1..length xs]] 