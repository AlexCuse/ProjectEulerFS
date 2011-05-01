module ProjectEuler.Problem29

#light
open System

let solve =
    seq{ for a in 2L .. 100L do
            for b in 2L .. 100L do
                yield a, b }
    |> Seq.map(fun(a, b) -> Math.Pow(float a, float b))
    |> Seq.distinct
    |> Seq.length