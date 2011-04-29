#light
module ProjectEuler.Problem16

open System
open System.Numerics
//need to figure out how to do without big integer (there's a trick for calculation powers of two)

let num = BigInteger.Pow(new BigInteger(2.0), 1000)

let answer = num.ToString().ToCharArray() |> Seq.fold (fun acc value -> acc + Int32.Parse(value.ToString())) 0

let solve = 
    answer