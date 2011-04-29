module ProjectEuler.Problem20

#light
open System
open System.Numerics

let rec factorial (start:BigInteger) =
    if start = new BigInteger(1) then
        start
    else
        start * (factorial (start - new BigInteger(1)))

let num = factorial (new BigInteger(100))


let answer = num.ToString().ToCharArray() |> Seq.fold (fun acc value -> acc + Int32.Parse(value.ToString())) 0

let solve = 
    answer
