#light
module ProjectEuler.Problem3

open System
open System.Collections.Generic

let solve =
    let x = 600851475143.0
    let p = Convert.ToInt64( Math.Ceiling( Math.Sqrt( x ) ) )
    let primes = PrimeFinder.primes( p )

    let factors = primes |> List.filter ( fun n -> x % (float)n = 0.0)
    factors|>List.max
