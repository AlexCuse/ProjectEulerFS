module ProjectEuler.Problem53

#light

open BigIntUtil

let nCr n r = 
    (factorial n)/((factorial r) * (factorial (n - r)))

let combinations n =
    [1I..n] //where r ≤ n
        |> List.map (fun r -> nCr n r)
         
let solve =
    [2I..100I]
        |> Seq.collect (fun n -> combinations n)
        |> Seq.filter (fun n -> n > 1000000I)
        |> Seq.length