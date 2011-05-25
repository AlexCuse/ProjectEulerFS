module ProjectEuler.Problem53

#light

let factorial n = 
    //can't pattern match on bigint
    if n < 1I then 1I
    else [n..(-1I)..1I]|> List.reduce (*)

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