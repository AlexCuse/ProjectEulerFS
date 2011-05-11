module ProjectEuler.Problem41

#light

open System

let solve = 
    Util.permutations [7..-1..1] Set.empty
        |> Seq.map Util.toNumber
        |> Seq.filter Util.isPrime
        |> Seq.head
