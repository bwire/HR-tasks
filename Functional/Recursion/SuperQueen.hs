-- Super-Queens on a Chessboard

import Data.List
import qualified Data.Set as S

type Cell = (Int, Int)

getDiagColNums :: Int -> Int -> Cell -> [Int]
getDiagColNums bound row (r, c) = 
  let dev = row - r
  in filter (\(column) -> column > 0 && column <= bound) $ (c - dev) : (c + dev) : []

getKnightColNums :: Int -> Int -> Cell -> [Int]
getKnightColNums bound row (r, c) = filter (\(column) -> column > 0 && column <= bound) $ 
  case (row - r) of 
    1 -> [c - 2, c + 2]
    2 -> [c + 1, c - 1]
    _ -> []

getFreeRowCells :: [Cell] -> Int -> Int -> [Cell]
getFreeRowCells cells row bound = 
  let
    set = S.fromList [1..bound]
    cs = foldl (\acc cell -> S.delete (snd cell) acc) set cells
    csd = foldl (\acc cell -> let cnums = (getDiagColNums bound row cell) in foldl (\aci col -> S.delete col aci) acc cnums) cs cells
    csdk = foldl (\acc cell -> let cnums = (getKnightColNums bound row cell) in foldl (\aci col -> S.delete col aci) acc cnums) csd cells
  in map ((,) row) (S.toList csdk)
  
getPlot :: [Cell] -> Int -> [[Cell]]
getPlot cells bound =
  let 
    rn = (fst $ head cells) + 1 
    fcells = getFreeRowCells cells rn bound
  in if null fcells && rn /= bound
  then [[]]
  else    
    if rn == bound 
    then map (\c -> c:cells) fcells
    else concatMap (\c -> getPlot (c:cells) bound) fcells

main :: IO ()
main = do
  dimension <- return . read =<< getLine
  let plot = concat [getPlot [(1, i)] dimension | i <- [1..dimension]]
  putStrLn $ show $ length (filter (not . null) plot)