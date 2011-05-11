module ProjectEuler.Problem43

#light

let toNumber digits = //as long
    digits |> Seq.fold (fun acc x -> acc * 10L + x) 0L

let sevenPrimes =
    Seq.unfold (fun n -> Some(n, n+1)) 1
    |> Seq.filter Util.isPrime
    |> Seq.take 7
    |> Array.ofSeq

(*
sequence within digits ([2..4]..[8..10])
convert digit subsets to number
first number should be divisible by first prime, second by second .. seventh by seventh
*)
let curious digits =  
    let arr = digits |> Array.ofList
    let check x =
        (arr.[x..x+2] |> toNumber) % (int64 sevenPrimes.[x-1]) = 0L
    
    [1..digits.Length - 3]|> List.forall check   

let solve =
    Util.permutations [0L..9L] Set.empty
    |> Seq.filter curious
    |> Seq.map toNumber
    |> Seq.sum