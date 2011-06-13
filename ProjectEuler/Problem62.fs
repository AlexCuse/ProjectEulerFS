module ProjectEuler.Problem62

#light

open System
open LongUtil

let cubeRoot (n:int64) =
    let n' = float n
    Math.Pow(n', (1.0/3.0))

let candidates n =
    //limit to cubes for n digits
    let min = int64(cubeRoot (pown 10L n))
    let max = int64(cubeRoot (pown 10L (n + 1)))

    [min..max] 
        |> List.map (fun n -> pown n 3)
        |> Seq.groupBy (fun n -> n |> digitsFrom |> List.sort)
        |> Seq.filter (fun (k, v) -> v |> Seq.length = 5)

let solve =
    Seq.unfold (fun n -> Some(n, n+1)) 1 //number of digits being checked, could probably start higher
        |> Seq.map candidates
        |> Seq.filter (fun sq -> (sq |> Seq.length) > 0)
        |> Seq.head
        |> Seq.map (fun (k, v) -> v |> Seq.min)
        |> Seq.min