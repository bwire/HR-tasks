-- Reverse a list

-- You are given a list of  elements. Reverse the list without using the reverse function. 
-- The input and output portions will be handled automatically. You need to write a function with the recommended method signature.

foldl (flip (:)) []