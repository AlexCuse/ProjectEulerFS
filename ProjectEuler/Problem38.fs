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
    let getUpperBound n =
        let x = float (n |> digitsFrom |> List.length)
        //int (x |> (/) 9.0 |> Math.Ceiling)
        int ((9.0/x) |> Math.Ceiling)
    seq {
        for i in 1 .. 99999 do
            for j in 1 .. getUpperBound i do
                yield [1 .. j]
                    |> List.fold (fun acc x -> acc + (i * x).ToString()) ""
    }

let solve =
    vals 
    |> Seq.filter (fun x -> pandigital x)
    |> Seq.max

