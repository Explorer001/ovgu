import Debug.Trace

--
-- Trace takes a string to print and a function to call. It returns
-- the return value of that function. Not pure since it prints to screen.
-- https://wiki.haskell.org/Pure
--

fib :: Int -> Int
fib 0 = trace "fib(0)" 0
fib 1 = trace "fib(1)" 1
fib n = trace ("fib(" ++ show (n-1) ++ ") + fib(" ++ show (n-2) ++ ")") fib (n-1) + fib (n-2)
