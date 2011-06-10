module ProjectEuler.Problem60

#light

open LongUtil

let primes = 
    PrimeFinder.primes 10000L //arbitrary limit
        |> List.filter (fun n -> not (n = 2L) && not (n = 5L)) //2 and 5 can't be concatenated to anything and remain prime

let concatMakesPrime (a, b) =
    let check (a, b) = 
        ((a |> digitsFrom) @ (b |> digitsFrom)) |> toNumber |> isPrime
        && ((b |> digitsFrom) @ (a |> digitsFrom)) |> toNumber |> isPrime
    if (a = b) then 
        false
    else
        Memoizer.memoize check (a,b)

let combos =
    seq {
          for a in primes do
            for b in (primes |> List.filter (fun n -> concatMakesPrime (n, a))) do
                for c in (primes |> List.filter (fun n -> concatMakesPrime (n, a) && concatMakesPrime (n, b))) do
                    for d in (primes |> List.filter (fun n -> concatMakesPrime (n, a) && concatMakesPrime (n, b) && concatMakesPrime (n, c))) do
                        for e in (primes |> List.filter (fun n -> concatMakesPrime (n, a) && concatMakesPrime (n, b) && concatMakesPrime (n, c) && concatMakesPrime (n, d))) do
                            yield [|a;b;c;d;e|]
    }

let solve =
    combos 
        |> Seq.map (fun arr -> arr, arr |> Array.sum) //|> Seq.minBy (fun (_,sum) -> sum) // takes 13 min (with check below) vs 10 sec
        |> Seq.head//kind of cheating to assume it will be the first encountered, but it is


(*
    //not sure how well this works
    let found = Set.empty
    let alreadyFound arr =
        let key = arr |> array.Sort
        if not (found.Contains key) then
            found.Add key |> ignore
            false
        else 
            true
*)