module ProjectEuler.Problem65

#light

open BigIntUtil

//e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...]
// this covers all but first term (2)
let eterms n =
    let ph = ref 0I
    seq {
        for i in 1I..n do
            if (i + 1I) % 3I = 0I then
                ph := !ph + 2I
                yield !ph
            else
                yield 1I
    }

let solve =
    eterms 99I 
    |> List.ofSeq
    |> List.rev //work from inside out
    |> List.fold (fun f n -> 1N/((n |> BigRational.FromBigInt) + f)) (0N/1N)
    |> fun frac -> (frac + (2N/1N)).Numerator |> digitsFrom |> List.sum //straight addition for first term outside

        