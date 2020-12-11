import Debug.Trace

--
-- Trace takes a string to print and a function to call. It returns
-- the return value of that function. If the function is pure, then
-- trace is pure.
--

fib :: Int -> Int
fib 0 = trace "fib(0)" 0
fib 1 = trace "fib(1)" 1
fib n = trace ("fib(" ++ show n ++ ")") fib (n-1) + fib (n-2)
