module ProjectEuler.Problem48

#light

open System.Numerics

let solve =
    Seq.unfold (fun n -> Some(n, n + 1I)) 1I
    |> Seq.take 1000
    |> Seq.map (fun n -> pown n (int n))
    |> Seq.sum
    |> string
    |> fun str -> str.Substring(str.Length - 10)
