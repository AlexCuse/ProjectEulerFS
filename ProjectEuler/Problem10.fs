#light
module ProjectEuler.Problem10

open System

let solve =
    PrimeFinder.primes ( Convert.ToInt64(2000000) )
    |> List.sum
