module ProjectEuler.Problem55

#light

let digitsFrom n =
    let rec decomposeFunc n listSoFar =
        if n = 0I then listSoFar
        else decomposeFunc (n / 10I) ([n % 10I] @ listSoFar)
    decomposeFunc n []

let toNumber digits =
    digits |> Seq.fold (fun acc x -> acc * 10I + x) 0I

let lychrel n =
    let compute ntc = (ntc |> digitsFrom |> List.rev |> toNumber) + ntc
    let isPalindrome ntc = (ntc |> digitsFrom |> List.rev) = (ntc |> digitsFrom)
    let rec tryComputePalindrome n1 att =
        let comp = compute n1
        if isPalindrome comp then false
        else
            if att < 50 then tryComputePalindrome comp (att + 1)
            else true
    tryComputePalindrome n 0

let solve = 
    [1I..10000I]
        |> List.filter lychrel
        |> List.length

