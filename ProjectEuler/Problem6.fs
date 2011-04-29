#light
module ProjectEuler.Problem6

open System

let sumOfSquares stop =
    [1 .. stop]|>List.map(fun x -> x * x)|>List.sum
let squareOfSum stop =
    let p = [1 .. stop]|>List.sum
    p * p
    
let diff stop = 
    sumOfSquares stop - squareOfSum stop
    
let solve =
    diff 100
    
