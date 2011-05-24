module ProjectEuler.Problem51

#light

open Util

let primes = PrimeFinder.primes 200000L |> List.map int

let rec digitCombinations x digits =
    match x, digits with
    | 0, _ -> [[]]
    | _, [] -> []
    | z, (v::w) -> List.map ((@) [v]) (digitCombinations (z-1) w) @ (digitCombinations z w)

    //this doesn't work, seeming equivalent above does
    //| z, (v::w) -> (digitCombinations (z-1) w) @ (digitCombinations z w) |> List.map (fun item -> [v] @ item)

let replacements numDigits = 
    [1..numDigits - 1] |> List.collect (fun x -> digitCombinations x [0..numDigits-1])
       
let replaceDigits digits n =
    let nDigits = n.ToString().ToCharArray()
    [0..9]
    |> List.map (fun x ->
        List.init nDigits.Length (fun y ->
            if List.exists (fun z -> z = y) digits
            then x.ToString()
            else nDigits.[y].ToString())
        |> List.reduce (+))
    |> List.map int
                
let primeReplacements n len =
    replacements (n |> digitsFrom |> List.length)
        |> List.map (fun x ->
            replaceDigits x n
            |> List.filter (fun y -> (n |> digitsFrom |> List.length) = (y |> digitsFrom |> List.length) && Util.isPrime y))
        |> List.filter (fun lst -> lst.Length >= len)
     
let solve =
    primes 
        |> Seq.skipWhile (fun n -> n <= 56003)
        |> Seq.map (fun n -> primeReplacements n 8)
        |> Seq.filter (fun l -> l.Length > 0)
        |> Seq.head
        |> List.map (fun lst -> List.min lst)
        |> Seq.min

