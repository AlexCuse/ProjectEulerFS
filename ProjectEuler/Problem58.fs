module ProjectEuler.Problem58

#light

open Util

let ringSizes =
    Seq.unfold (fun n -> Some(n, n+2)) 1

let corners n =
    //0 to 3 for each corner
    let positionCalc n v =
        (n * n) - (v * (n - 1))
    [3..-1..0] |> List.map (fun c -> positionCalc n c)

let solve =
    let countPrimes x =
        x |> List.fold(fun acc n -> if (isPrime n) then acc + 1 else acc ) 0

    ringSizes
    |> Seq.zip (ringSizes |> Seq.map corners)
    |> Seq.scan (fun (pcount, sz) (x, y)-> pcount + (x |> countPrimes), y) (0,0)
    |> Seq.skipWhile (fun (pcount, sz) -> pcount = 0 || ((float pcount) / (float (sz * 2))) > 0.1)
    |> Seq.head