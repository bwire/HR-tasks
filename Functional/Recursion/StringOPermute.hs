-- String-o-Permute

import Control.Monad (replicateM, mapM_, liftM)

sop :: String -> String
sop [] = []
sop [x] = [x]
sop (x1:x2:xs) = x2:x1:sop xs

main :: IO ()
main = do
  n <- liftM read getLine
  lines <- replicateM n getLine
  mapM_ (putStrLn . sop) lines