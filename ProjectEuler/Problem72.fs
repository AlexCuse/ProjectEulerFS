module ProjectEuler.Problem72

#light

open LongUtil

let primes = PrimeFinder.primes 1000000L

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

let solve =
    seq {
        for m in 2L..1000000L do
            yield phi m
    } |> Seq.sum