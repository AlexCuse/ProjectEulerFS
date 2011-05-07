module ProjectEuler.Util

#light
open System

//http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let rec greatestCommonDivisor a b =
    match b with
    | b when b = 0 -> a
    | b -> greatestCommonDivisor b (a % b)

let factors x =
    match x with
        | _ when x < 0 -> failwith "no factors, fool!"
        | 0 -> [] | 1 -> [1]
        | _ ->
            let sqrt = int (Math.Sqrt((float x)))

            let factorPairs = 
                [1 .. sqrt - 1] 
                    |> Seq.filter(fun p -> x % p = 0) 
                    |> Seq.map(fun p -> (p, x / p))

            let (lowFactors, highFactors) = factorPairs |> List.ofSeq |> List.unzip

            // x is a perfect square
            if sqrt * sqrt = x then
                lowFactors @ [ sqrt ] @ (highFactors |> List.rev)
            // if floor(sqrt) is a factor
            elif x % sqrt = 0 then
                lowFactors @ [ sqrt; x / sqrt ] @ (highFactors |> List.rev)
            else
                lowFactors @ (highFactors |> List.rev)

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        match n with 
        | 0 -> listSoFar
        | _ -> decomposeFunc (n / 10) ([n % 10] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Array.fold (fun acc x -> acc * 10 + x) 0