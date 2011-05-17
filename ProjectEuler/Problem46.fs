module ProjectEuler.Problem46

#light

open Util

let oddComposites =
    Seq.unfold (fun x -> Some(x, x + 2)) 3
    |> Seq.filter (fun x -> not(isPrime x))

let primes =
    PrimeFinder.primes 1000000L
    |> List.map int

let goldbach n =
    primes 
    |> Seq.takeWhile(fun x -> x < n)
    |> Seq.exists (fun x -> sqrt(double((n - x) / 2)) % 1.0 = 0.0)

let solve =
    oddComposites
    |> Seq.filter (fun n -> not(goldbach n))
    |> Seq.head

