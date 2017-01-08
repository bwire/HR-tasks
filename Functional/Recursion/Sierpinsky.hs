import Control.Monad(mapM_)

data Triangle = Full Int | Dissected Int Triangle Triangle Triangle

-- big triangle columns number
max_found = 63

-- build triangle with initial foundation and a given depth
buildTriangle :: Int -> Triangle
buildTriangle n = build' max_found n where
  build' found 0 = Full found
  build' found n = let new_found = (found - 1) `div` 2
                       new_triangle = build' new_found (n - 1) 
                   in Dissected new_found new_triangle new_triangle new_triangle                        

-- wrap the line of a triangle with leading and trailing '_'s                  
wrapString :: Int -> String -> String
wrapString found s = 
  let len = (found - length s) `div` 2
      side = replicate len '_'
  in side ++ s ++ side 

drawTriangle :: Triangle -> [String]
drawTriangle (Full f) = map (wrapString f . (flip replicate) '1')[n | n <- [1..f], n `mod` 2 == 1]
drawTriangle (Dissected f u l r) = 
  let 
      lower_part = zipWith (++) (drawTriangle l) (map ('_':) (drawTriangle r))
      upper_part = map (wrapString (2 * f + 1)) (drawTriangle u)
  in upper_part ++ lower_part

main :: IO ()
main = do
  n <- return . read =<< getLine
  mapM_ putStrLn (drawTriangle . buildTriangle $ n)