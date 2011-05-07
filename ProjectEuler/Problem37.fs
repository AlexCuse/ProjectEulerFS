module ProjectEuler.Problem37

#light

open Util
open PrimeFinder

let primes = 
    1000000L 
    |> primes
    |> Seq.map int
    |> Seq.skipWhile(fun x -> x < 23)//first candidate
    |> Set.ofSeq

let truncatable num =
    let d = num |> digitsFrom
    let rec truncations dgts =
        seq {
            match dgts with 
                | head :: tail -> 
                    yield dgts |> toNumber
                    yield! (truncations tail)
                | [] -> ()
        }

    (truncations d) |> Seq.forall (fun x -> primes.Contains x) 

let solve =
    0