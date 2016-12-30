-- Fibonacci Numbers

-- In this challenge, we learn about using the Fibonacci Function.

fib n | n == 0 = 0
      | n == 1 = 0
      | n == 2 = 1
      | otherwise = fib (n - 1) + fib (n - 2)

