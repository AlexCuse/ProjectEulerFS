#light
module ProjectEuler.Problem9

open System

let square x = x * x

let aNb = 
    seq{ for a in 1 .. 1000 do
            for b in a .. 1000 do
                yield a, b }
    |>Seq.filter( fun (a,b) -> square a + square b = square ( 1000 - a - b ) )
    |> Seq.head
    
    
let solve = 
    let a = fst aNb
    let b = snd aNb
    let c = 1000 - a - b
    a * b * c
