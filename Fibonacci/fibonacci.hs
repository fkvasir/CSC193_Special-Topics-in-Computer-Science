fib2 :: Int -> Int
fib2 n
    | n < 0     = error "Input must be a non-negative integer"     --If the input n is negative, it raises an error because Fibonacci is not defined for negative numbers.
    | n == 0    = 0     -- If n is 0, it returns 0 (the first Fibonacci number).
    | n == 1    = 1     -- If n is 1, it returns 1 (the second Fibonacci number).
    | otherwise = fibHelper n 0 1 2     --  For any other input, it calls the helper function fibHelper

    -- fibHelper n 0 1 2: The initial state is prev = 0, curr = 1 , and the index i = 2, which corresponds to the next Fibonacci number to be calculated.

fibHelper :: Int -> Int -> Int -> Int -> Int        -- performs the iterative calculation
fibHelper n prev curr i
    -- Here I have initialized 4 variables as parameters that stores values being passed:
    
    -- n -> The input number for which we want to calculate the Fibonacci number
    -- prev -> The Fibonacci number at position i-2
    -- curr -> The Fibonacci number at position i-1
    -- i -> The current position in the Fibonacci sequence (starting from 2)

    | i > n     = curr      -- This is the base case of the recursion. If the current index i exceeds n, it returns curr, which is the Fibonacci number at position n.
    | otherwise = fibHelper n curr (prev + curr) (i + 1)        -- The recursive case. It calculates the next Fibonacci number by updating prev to curr and curr to prev + curr, and then increments i by 1 to move to the next Fibonacci number. 

main :: IO ()       -- main function
main = do
    -- tests the fib2 function by printing the Fibonacci numbers for several values of n.
    print (fib2 0)  -- Output: 0
    print (fib2 1)  -- Output: 1
    print (fib2 5)  -- Output: 5
    print (fib2 10) -- Output: 55
