module ProjectEuler.Problem30

#light

open System;

let solve = 
    let pow = 5 //close around this
    let isSumDigitsToPower n =
        n.ToString()
            |> Seq.map (fun numChar ->
                        let num = Int32.Parse(numChar.ToString())
                        pown num pow)
            |> Seq.sum = n

    [2..(pow + 1) * (pown 9 pow)] 
        |> Seq.filter(isSumDigitsToPower)
        |> Seq.sum