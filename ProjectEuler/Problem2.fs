#light
module ProjectEuler.Problem2

open System
open System.Numerics

let solve =
    Seq.unfold (fun (n0, n1) -> Some(n0, (n1, n0 + n1))) (1I,1I)
        |> Seq.takeWhile( fun x-> x <= 4000000I )
        |> Seq.filter( fun x-> x % 2I = 0I )
        |> Seq.sum
        