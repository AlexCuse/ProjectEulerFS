module ProjectEuler.Problem34

#light

open Util

let factorial number =
    let rec factorialTail n tail =
        if n <= 1 then
            tail
        else
            factorialTail (n - 1) (tail * n)
    factorialTail number 1

let sumFactorials n =
    n
    |> digitsFrom
    |> List.fold (fun acc x -> acc + (factorial x)) 0

let solve =
    [3 .. factorial 10]//2540160
    |> Seq.filter(fun x -> x = sumFactorials x)
    |> Seq.sum 