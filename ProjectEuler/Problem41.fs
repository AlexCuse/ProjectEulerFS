module ProjectEuler.Problem41

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
    Util.permutations [7..-1..1] Set.empty
        |> Seq.map Util.toNumber
        |> Seq.filter primes.Contains
        |> Seq.head
