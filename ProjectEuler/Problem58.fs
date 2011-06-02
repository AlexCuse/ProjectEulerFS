module ProjectEuler.Problem58

#light
(*

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

let calcRing n =
    let nSquared = n * n 
    nSquared + (nSquared - (n - 1)) + (nSquared - (2 * (n - 1))) + (nSquared - (3 * (n - 1)))

*)

open Util

let ringSizes =
    Seq.unfold (fun n -> Some(n, n+2)) 1

let corners n =
    //0 to 3 for each corner
    let positionCalc n v =
        (n * n) - (v * (n - 1))
    [3..-1..0] |> List.map (fun c -> positionCalc n c)

let solve =
    ringSizes
    |> Seq.zip (ringSizes |> Seq.map corners)
    |> Seq.scan (fun (pcount, sz) (x, y)-> 
        pcount + (x |> List.fold(fun acc n -> if (isPrime n) then acc + 1 else acc ) 0)
        , y) (0,0)
    |> Seq.skipWhile (fun (pcount, sz) -> pcount = 0 || ((float pcount) / (float (sz * 2))) > 0.1)
    |> Seq.head