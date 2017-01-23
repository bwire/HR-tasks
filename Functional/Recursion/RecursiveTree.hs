-- Functions and Fractals - Recursive Trees 

import Control.Monad(mapM_)

rows = 63
columns = 100
foundation = 16

drawY :: Int -> [String]
drawY n = wrapToPlot $ drawY' foundation n where
  drawY' f n 
    | n > 0 =
      let trunk = wrapStrings $ getBlocks getTrunk f
          branches = wrapStrings $ getBlocks getBranches f
      in (drawY' (f `div` 2) (n - 1)) ++ branches ++ trunk
    | otherwise = []
    
getBlocks :: (Int -> [String]) -> Int -> [String]
getBlocks f fn = map (\s -> tail . concat $ replicate (foundation `div` fn) ('_':s)) (f fn)

getTrunk :: Int -> [String]
getTrunk l = let part = replicate (l * 2 - 1) '_' 
             in replicate l (part ++ ('1' : part))
             
getBranches :: Int -> [String]
getBranches l = 
  let branches = getBranches' l where
        getBranches' 0 = []
        getBranches' n = 
          let len = (n + 1) * 2 - 1
              lp = len - 2
              line = if lp < 0 then [] else (replicate lp '_') ++ "1" 
          in ('1':line):(getBranches' (n -1)) 
  in map mapper branches where
    mapper b = 
      let part = replicate ((l - 1) * 2 - (length b - 1) `div` 2 + 1) '_' 
      in part ++ b ++ part

  
wrapStrings :: [String] -> [String]
wrapStrings = map wrapperH where 
    wrapperH x = (replicate (part - 1) '_') ++ x ++ (replicate part '_') where
      len = length x
      part = (columns - len + 2) `div` 2 
    
wrapToPlot :: [String] -> [String]
wrapToPlot xs = (replicate (rows - length xs) (replicate columns '_')) ++ xs
  
main :: IO ()
main = do
  n <- return . read =<< getLine
  mapM_ putStrLn $ drawY n