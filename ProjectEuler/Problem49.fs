module ProjectEuler.Problem49

#light

open Util

let primeLookup = 
    PrimeFinder.primes 10000L
    |> List.map int
    |> List.filter (fun n -> n > 1000)

let sameDigits x y =
    (x |> digitsFrom |> List.sort) = (y |> digitsFrom |> List.sort)

let solve =
    seq {
        for cand in primeLookup do
            let matches = 
                primeLookup
                    |> List.filter (fun n -> sameDigits n cand)
        
            let chains =
                seq {
                    for x in matches  do
                        for y in (matches |> List.filter (fun n -> n <> x)) do
                            for z in (matches |> List.filter (fun n -> n <> x && n <> y)) do
                                if ( x < y && y < z ) then
                                    yield x, y, z
                }
        
            for (x, y, z) in chains do 
                if (y - x) = (z - y) then
                    yield x, y, z
    }
    |> Seq.distinct
    |> Seq.skip 1 //assume the one provided is the first encountered
    |> Seq.head
