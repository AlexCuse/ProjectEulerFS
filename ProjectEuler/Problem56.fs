module ProjectEuler.Problem56

#light

open Util

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        if n = 0I then listSoFar
        else decomposeFunc (n / 10I) ([n % 10I] @ listSoFar)
    decomposeFunc n []

let solve =
    seq {
        for a in [1I..100I] do
            for b in [1..100] do
                yield a ** b 
    } 
    |> Seq.map (fun n -> (digitsFrom n) |> List.sum)
    |> Seq.max