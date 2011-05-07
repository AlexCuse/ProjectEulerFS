module ProjectEuler.Problem34

#light

let digits n =
    let rec decomposeFunc n listSoFar =
        match n with 
        | 0 -> listSoFar
        | _ -> decomposeFunc (n / 10) (listSoFar @ [n % 10])
    decomposeFunc n []

let factorial number =
    let rec factorialTail n tail =
        if n <= 1 then
            tail
        else
            factorialTail (n - 1) (tail * n)
    factorialTail number 1

let sumFactorials n =
    digits n 
    |> List.fold (fun acc x -> acc + (factorial x)) 0

let solve =
    [3 .. factorial 10]//2540160
    |> Seq.filter(fun x -> x = sumFactorials x)
    |> Seq.sum 