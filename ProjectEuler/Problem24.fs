module ProjectEuler.Problem24

#light

open System;

let solve = 
    Util.permutations [0..9] Set.empty
        |> Seq.skip(999999)
        |> Seq.head