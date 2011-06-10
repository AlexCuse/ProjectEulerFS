module ProjectEuler.Util

#light
open System
open Microsoft.FSharp.Core.Operators

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

let isPrime n =
    let rec checkRemaining composite i num =
        if composite then false
            elif i*i > num then true else
                checkRemaining (num % i = 0 || num % (i + 2) = 0) (i + 6) num

    match n with
    | n when n<=1 -> false
    | 2 | 3 -> true
    | n' when n' &&& 1 = 0 || n' % 3 = 0 -> false //even or multiple of 3
    | n' ->  checkRemaining false 5 n'  

let factorial n = 
    if n < 1 then 1
    else [n..(-1)..1]|> List.reduce (*)

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        match n with 
        | 0 -> listSoFar
        | _ -> decomposeFunc (n / 10) ([n % 10] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Seq.fold (fun acc x -> acc * 10 + x) 0
