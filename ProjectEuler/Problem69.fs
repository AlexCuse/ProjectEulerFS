module ProjectEuler.Problem69

#light

(*

While attempting to find a 'fast-enough' brute-force method,
noticed that numbers divisible by the most small primes seem
to have the lowest values for phi (n).

Tried multiplying small consecutive primes to get the largest 
number possible less than a million, and this ends up being
the answer.

*)

let primes = PrimeFinder.primes 50L

let solve = 
    primes
    |> List.map (fun n ->
        primes 
            |> List.filter (fun n' -> n' < n)   
            |> List.fold (fun acc x -> acc * x) 1L
    )
    |> List.filter (fun n -> n <= 1000000L)
    |> List.max

