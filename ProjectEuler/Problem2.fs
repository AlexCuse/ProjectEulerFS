#light
module ProjectEuler.Problem2

open System
open System.Numerics

let sumEvenFibs =
    Seq.unfold (fun (n0, n1) -> Some(n0, (n1, n0 + n1))) (1I,1I)
        |> Seq.takeWhile( fun x-> x <= BigInteger.Parse("4000000") )
        |> Seq.filter( fun x-> x % new BigInteger(2) = BigInteger.Zero )
        |> Seq.sum
        