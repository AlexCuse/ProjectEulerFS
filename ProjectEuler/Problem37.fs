module ProjectEuler.Problem37

#light

open Util
open PrimeFinder

let primes = 
    1000000L 
    |> primes
    |> List.map int
    |> Set.ofSeq

let truncatable num =
    let d = num |> digitsFrom
    let rec leftTruncations dgts =
        seq {
            match dgts with 
                | head :: tail -> 
                    yield dgts |> toNumber
                    yield! (leftTruncations tail)
                | [] -> ()
        }
    
    let rec rightTruncations dgts =
        seq {
            match dgts with 
                | head :: tail -> 
                    yield dgts |> List.rev |> toNumber
                    yield! (rightTruncations tail)
                | [] -> ()
        }

    (leftTruncations d) |> Seq.forall (fun x -> primes.Contains x) &&
    (rightTruncations (d |> List.rev)) |> Seq.forall(fun x -> primes.Contains x)

let solve =
    primes
    |> Seq.skipWhile(fun x -> x < 23)//first candidate
    |> Seq.filter truncatable
    |> Seq.sum
