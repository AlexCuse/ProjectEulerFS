module ProjectEuler.Problem52

#light

open Util

let sameDigits x =
    [2..6]
        |> List.map (fun z -> z * x)
        |> List.map digitsFrom
        |> List.map List.sort
        |> Seq.distinct
        |> Seq.length = 1

let solve =
    Seq.unfold (fun x -> Some(x , x + 1)) 1
        |> Seq.filter sameDigits
        |> Seq.head