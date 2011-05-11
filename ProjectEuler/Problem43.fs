module ProjectEuler.Problem43

#light

let toNumber digits = //as long
    digits |> Seq.fold (fun acc x -> acc * 10L + x) 0L

let curious digits =
    let sevenPrimes = 
        Seq.unfold (fun n -> Some(n, n+1)) 1
        |> Seq.filter Util.isPrime
        |> Seq.take 7
        |> Array.ofSeq

    let arr = digits |> Array.ofList
    [1..digits.Length - 3]
    |> List.forall (fun x -> (arr.[x..x+2] |> toNumber) % (int64 sevenPrimes.[x-1]) = 0L)   

let solve =
    Util.permutations [0L..9L] Set.empty
    |> Seq.filter curious
    |> Seq.map toNumber
    |> Seq.sum