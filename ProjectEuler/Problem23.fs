module ProjectEuler.Problem23

#light
open System;

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

let isAbundant x =
    let sum = factors x |> Seq.fold (+) 0
    (sum - x > x)

let abundantNumberCheck = [| for x in 0 .. 28123 -> isAbundant x |]

let abundantList = [1 .. 28123] |> List.filter (fun x -> isAbundant x)

let rec isPairOfAbundants list x =
    if (List.head list) > x then false
    elif abundantNumberCheck.[x - (list |> List.head)] then true
    else isPairOfAbundants (list |> List.tail) x       

let nonPairOfAbundants = 
    [1 .. 28123] 
        |> List.filter (fun num -> not(isPairOfAbundants abundantList num)) 

    // Return their sum
let solve = 
    nonPairOfAbundants |> List.fold (+) 0 
