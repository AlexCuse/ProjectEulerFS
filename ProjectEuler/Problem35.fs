module ProjectEuler.Problem35

#light
open Util
open PrimeFinder

let primes = 
    primes 1000000L 
    |> List.map int 
    |> Set.ofList

let rotations n =
    let digits = 
        n |> digitsFrom |> List.toArray

    let rotate shift dg =
        let innerRotate =
            let len = dg |> Array.length
            if shift % len = 0 then dg
            else dg.[shift..(len - 1)] |> Array.append dg.[..(shift - 1)]
        innerRotate |> Array.toList
        
    seq {
        for i in 1 .. (digits.Length - 1) do
            yield
                digits
                |> rotate i
                |> toNumber
    }

let isCircular n =
    (rotations n) |> Seq.forall (fun x -> primes |> Set.contains x)

let solve = 
    primes
    |> Seq.filter isCircular
    |> Seq.length