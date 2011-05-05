module ProjectEuler.Problem33

#light

let pairs = 
    seq {
        for numerator in 10..99 do
            for denominator in 10..99 do
                if not (numerator = denominator || numerator > denominator || numerator % 10 = 0 || denominator % 10 = 0) then
                    yield numerator, denominator
    }

let decompose num =
    num / 10, num % 10
    
let isCuriousFraction numerator denominator =    
    let compareNormalToReduced num den =
        let divideInts x y =
            double x / double y
        divideInts num den = divideInts numerator denominator

    let n = decompose numerator
    let d = decompose denominator
    
    match n, d with
        | _ when fst(n) = snd (d) -> compareNormalToReduced (snd(n)) (fst(d))
        | _ when snd(n) = fst (d) -> compareNormalToReduced (fst(n)) (snd(d))
        | _ -> false

let solve = 
    pairs 
    |> Seq.filter (fun (x, y) -> isCuriousFraction x y)
    |> Seq.fold (fun acc x -> fst(x) * fst(acc), snd(x) * snd(acc)) (1, 1) 
    |> fun (x, y) ->  y / (Util.greatestCommonDivisor x y)
