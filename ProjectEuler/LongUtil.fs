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

//shapes
let isPentagon n =
    ((sqrt(float (24L * n + 1L)) + 1.0) % 6.0) = 0.0

let triangle n =
    (n * (n+1L)) / 2L

let square n =
    n * n

let pentagon n =
    (n * ((3L * n) - 1L)) / 2L

let hexagon n =
    n * ((2L * n) - 1L)

let heptagon n =
    (n * ((5L*n) - 3L)) / 2L

let octagon n =
    n * ((3L * n) - 2L)
