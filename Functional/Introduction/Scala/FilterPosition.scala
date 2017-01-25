-- Filter Positions in a List 
-- For a given list with  integers, return a new list removing the elements at odd positions. 
-- The input and output portions will be handled automatically. You need to write a function with the recommended method signature.

def f(arr: List[Int]): List[Int] = {
  (List.range(1, arr.length + 1) zip arr)
    .filter(_._1 % 2 == 0)
    .map(_._2)
}