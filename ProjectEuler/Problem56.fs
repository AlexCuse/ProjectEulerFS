module ProjectEuler.Problem56

#light

open BigIntUtil

let solve =
    seq {
        for a in [1I..100I] do
            for b in [1..100] do
                yield a ** b 
    } 
    |> Seq.map (fun n -> (digitsFrom n) |> List.sum)
    |> Seq.max