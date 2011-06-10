module ProjectEuler.LongUtil

#light

let isPrime n =
    let rec checkRemaining composite i num =
        if composite then false
            elif i*i > num then true else
                checkRemaining (num % i = 0L || num % (i + 2L) = 0L) (i + 6L) num

    match n with
    | n when n<=1L -> false
    | 2L | 3L -> true
    | n' when n' &&& 1L = 0L || n' % 3L = 0L -> false //even or multiple of 3
    | n' ->  checkRemaining false 5L n'  

let factorial n = 
    if n < 1L then 1L
    else [n..(-1L)..1L]|> List.reduce (*)

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        match n with 
        | 0L -> listSoFar
        | _ -> decomposeFunc (n / 10L) ([n % 10L] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Seq.fold (fun acc x -> acc * 10L + x) 0L
