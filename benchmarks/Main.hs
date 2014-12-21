module Main ( main ) where

import Criterion.Main
    (   bench
    ,   bgroup
    ,   defaultMain
    ,   nf
    ,   whnf
    )
import Criterion.Types ( Pure )

-- | An example function to benchmark...
--
fib :: Int -> Int
fib 0 = 0
fib 1 = 0
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = defaultMain
    -- when benchmarking **pure** functions, use one of the following two functions to produce a Benchmark:
    -- @ nf :: (a -> b) -> a -> Pure @ evaluates the given expression with the given argument to normal form
    -- @ whnf :: (a -> b) -> a -> Pure @ evaluates the given expression with the given argument to weak head normal form
    -- you can group benchmarks together with `bgroup`, benchmarks groups can contain benchmarks groups also
    [   bgroup "Example Group 1"
        [   bench "fib 10 (Normal Form)" $ nf (const (fib 10)) ()
        ,   bench "fib 10 (Weak Head Normal Form)" $ whnf (const (fib 10)) ()
        ]
    ,   bgroup "Example Group 2"
        [   bench "fib 20 (Normal Form)" $ nf (const (fib 20)) ()
        ,   bench "fib 20 (Weak Head Normal Form)" $ whnf (const (fib 20)) ()
        ]
    ]
