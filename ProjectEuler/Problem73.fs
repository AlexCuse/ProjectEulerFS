#light

module ProjectEuler.Problem73

// found trick to directly calculate farey sequence length (for section between 1/minD and 1/maxD, exclusive) here: 
//   http://freelancersunite.net/project_euler/project-euler-problem-73/ 

let rec partialFareyLength minD maxD max =
    let dsum = minD + maxD
    if dsum > max then
        0
    else        
        1 + (partialFareyLength minD dsum max) + (partialFareyLength dsum maxD max)

let solve =
    partialFareyLength 2 3 12000

(*
// Using full sequence takes ~2 min (on slowish machine)

//TODO: tweaking this to enable retrieving partial sequence (with min/max denominator) could be interesting
let farey limit =
    let bigRatify n d = BigRational.FromInt(n) / BigRational.FromInt(d)
    seq {
        let p = ref 0
        let q = ref 1
        let p' = ref 1
        let q' = ref limit
        yield bigRatify !p !q
        while not (!p = 1 && !q = 1) do
            let c = (!q + limit) / !q'
            let p'' = c * !p' - !p
            let q'' = c * !q' - !q
            p := !p'
            q := !q'
            p' := p''
            q' := q''
            yield bigRatify !p !q }

let solve =
    farey 12000
    |> Seq.filter (fun br -> br > 1N/3N && br < 1N/2N)
    |> Seq.length

*)




(*

// Full Farey Length = 1 + (sum totient for 1 to limit)

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

let flen n =
    [1L..n] |> Seq.fold (fun acc m -> acc + (phi m)) 1L


*)
