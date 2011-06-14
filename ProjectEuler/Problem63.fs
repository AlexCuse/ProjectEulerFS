module ProjectEuler.Problem63

#light

open BigIntUtil

let nums = Seq.unfold (fun n -> Some(n, n+1)) 1

let countNumbersOfNDigitsNthPower numDigits =
    nums
        |> Seq.map (fun n -> pown (bigint n) numDigits)
        |> Seq.skipWhile (fun n -> n |> digitsFrom |> List.length < numDigits)
        |> Seq.takeWhile (fun n -> n |> digitsFrom |> List.length = numDigits)
        |> Seq.length

let solve =
    nums
        |> Seq.map countNumbersOfNDigitsNthPower
        |> Seq.takeWhile (fun cnt -> cnt > 0) //eventually it has to stop right?
        |> Seq.sum
    

