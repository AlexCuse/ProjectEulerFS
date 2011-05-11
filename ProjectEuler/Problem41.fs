﻿module ProjectEuler.Problem41

#light

open System

//let isPrime n =
//    let max = int (Math.Sqrt(float n))
//    not ( { 2 .. max } 
//        |> Seq.filter (fun div -> n % div = 0) 
//        |> Seq.isEmpty )

//learned the hard way that there's no 9 or 8 digit primes...
let primes = PrimeFinder.primes 7654321L |> List.map int |> Set.ofList

let solve = 
    Util.permutations [1..7] Set.empty
        |> Seq.map Util.toNumber
        |> List.ofSeq
        |> List.sortBy (fun x -> -1 * x)
        |> Seq.filter primes.Contains
        |> Seq.head