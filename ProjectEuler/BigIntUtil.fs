module ProjectEuler.BigIntUtil

#light

let factorial n = 
    //can't pattern match on bigint
    if n < 1I then 1I
    else [n..(-1I)..1I]|> List.reduce (*)

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        if n = 0I then listSoFar
        else decomposeFunc (n / 10I) ([n % 10I] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Seq.fold (fun acc x -> acc * 10I + x) 0I

let isPrime n =
    let rec checkRemaining composite i num =
        if composite then false
            elif i*i > num then true else
                checkRemaining (num % i = 0I || num % (i + 2I) = 0I) (i + 6I) num

    if n<=1I then false
    elif n = 2I || n = 3I then true
    elif n &&& 1I = 0I || n % 3I = 0I then false //even or multiple of 3
    else  checkRemaining false 5I n  

