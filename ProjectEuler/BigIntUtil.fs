module ProjectEuler.BigIntUtil

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        if n = 0I then listSoFar
        else decomposeFunc (n / 10I) ([n % 10I] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Seq.fold (fun acc x -> acc * 10 + x) 0

let isPrime n =
    let rec checkRemaining n r f =
        if f >= r then true
        elif n % f = 0I then false
        elif n % (f + 2I) = 0I then false
        else checkRemaining n r (f + 6I)
        
    if n = 1I then false
    elif n < 4I then true
    elif n % 2I = 0I then false
    elif n < 9I then true
    elif n % 3I = 0I then false
    else
        let r = bigint ((float n) |> sqrt |> ceil)
        checkRemaining n r 5I

