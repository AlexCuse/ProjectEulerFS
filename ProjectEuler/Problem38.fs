module ProjectEuler.Problem38

#light

open Util
open System

let pandigital (n:string) =
    if n.Length <> 9 then false
    else
        let digits = (int n) |> digitsFrom |>Set.ofList
        let allDigits = [1..9]

        allDigits |> List.forall (fun x -> digits.Contains x)

let vals = 
    seq {
        for i in 1 .. 99999 do
            yield [1 .. 2]//2 should work, may need to try [1..3], [1..4], etc... 
                |> List.fold (fun acc x -> acc + (i * x).ToString()) ""
    }

let solve =
    vals 
    |> Seq.filter (fun x -> pandigital x)
    |> Seq.max

