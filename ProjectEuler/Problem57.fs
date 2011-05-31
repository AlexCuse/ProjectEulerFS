module ProjectEuler.Problem57

#light
 
open BigIntUtil

//√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

let fractionSequence num =
    Seq.unfold (fun (n, d) -> Some((n, d), (d * 2I + n, d + n))) (3I, 2I)
    |> Seq.take num
 
let solve =
    fractionSequence 1000
    |> Seq.filter (fun (n, d) -> (n |> digitsFrom |> List.length) > (d |> digitsFrom |> List.length))
    |> Seq.length