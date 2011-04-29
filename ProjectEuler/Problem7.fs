#light
module ProjectEuler.Problem7

open System

let primes =
    PrimeFinder.primes ( Convert.ToInt64 1000000 )
    
let solve = 
    10000|>List.nth primes