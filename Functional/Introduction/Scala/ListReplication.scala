-- List Replication

-- Given a list, repeat each element in the list  amount of times. 
-- The input and output portions will be handled automatically by the grader. 
-- You need to write a function with the recommended method signature.

def f(num: Int, arr: List[Int]): List[Int] = {
    for (i <- 0 until arr.length)
	   yield {
			for (n <- 1 to num) yield arr(i)
		}
}.toList.flatten