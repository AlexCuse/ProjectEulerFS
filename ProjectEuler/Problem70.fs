module ProjectEuler.Problem70

#light

open System.Collections.Generic
open LongUtil

//primes up to square root of 10^7
let primes = PrimeFinder.primes 10000L

let primeFactors n =
    let rec getPrimeFactorsRec factors n =
        match n with
        | 1L -> factors |> Seq.distinct
        | _ ->
            let factor = primes |> Seq.filter (fun x -> n % x = 0L) |> Seq.head
            getPrimeFactorsRec (factors @ [factor]) (n/factor)
    getPrimeFactorsRec [] n
 
//totient(n) = n * { product(all prime factors - 1) } / { product (all prime factors) }
let phi n =
    match n with
    | 1L -> 1L
    | n when (isPrime n) -> n - 1L
    | _ ->
        let pfact = primeFactors n
        n * 
        (pfact 
            |> Seq.map (fun n' -> n'-1L) 
            |> Seq.reduce (*)) 
        / (pfact |> Seq.reduce (*))

let isPermutation i1 i2 =
    (i1 |> digitsFrom |> List.sort) = (i2 |> digitsFrom |> List.sort)

//this reduction of problem space to prime * prime values keeps perf reasonable
let solve =
    primes
    |> Seq.collect (fun n ->
        primes
        |> Seq.filter (fun n' -> n' > n)
        |> Seq.map (fun n' -> n * n'))
        |> Seq.filter (fun n' -> n' > (pown 10L 6) && n' < (pown 10L 7))
    |> Seq.map (fun n -> (n, phi n))
    |> Seq.filter (fun (n, n') -> isPermutation n n')
    |> Seq.minBy (fun (n, n') -> (double n) / (double n'))


 
