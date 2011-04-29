#light
module ProjectEuler.Problem12

open System

//how to get w/ Seq.unfold?
let triangles =
    let rec getTriangles n start = 
        seq {   let tri = n + start
                yield tri
                yield! getTriangles (n + 1) tri }
    getTriangles 1 0

let divisorsCount num = 
    let rec getDivisors divisor number =
        if divisor = 1 then [1]
        elif number % divisor = 0 then divisor :: number / divisor :: (getDivisors (divisor - 1) number)
        else (getDivisors (divisor - 1) number)
    (getDivisors (int(sqrt(float num))) num)
    //|> Seq.distinct//this doubles execution time
    |> Seq.length

let solve = 
    //triangles|>Seq.filter(fun v -> (divisorsCount v) >= 500)|>Seq.head
    triangles|>Seq.skipWhile(fun v -> (divisorsCount v) <= 500)|>Seq.head