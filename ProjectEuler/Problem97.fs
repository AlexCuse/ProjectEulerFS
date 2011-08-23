module ProjectEuler.Problem97

#light

let bpow = System.Numerics.BigInteger.Pow

let lastTenDigitsFrom n =
    let rec decomposeFunc n listSoFar =
        if n = 0I || ((listSoFar |> List.length) = 10) then listSoFar
        else decomposeFunc (n / 10I) ([n % 10I] @ listSoFar)
    decomposeFunc n []

let solve =
    let num = 28433I * bpow(2I, 7830457) + 1I
    lastTenDigitsFrom num
    