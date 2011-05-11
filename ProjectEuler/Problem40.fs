module ProjectEuler.Problem40

#light

open Util

let strangeDigits =
    Seq.unfold (fun x -> Some(x, x + 1)) 1 //all positive integers
    |> Seq.collect (fun num -> (num |> digitsFrom)) //all digits of each

let digit n =
    strangeDigits
    |> Seq.nth (n - 1)

let solve =
    [0..6] 
        |> List.map (fun n -> digit (pown 10 n)) 
        |> List.reduce (fun acc x -> acc * x)  //could use fold here, but this may describe operation better