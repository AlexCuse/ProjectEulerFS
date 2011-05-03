module ProjectEuler.Problem32

#light

let deconstruct lst idx1 idx2 =
    let get arr =
        arr |> Array.fold (fun x acc-> (x * 10) + acc) 0
    let arr = lst |> List.toArray
    let finIdx = (arr |> Array.length) - 1
    get (arr.[0..idx1 - 1]), get (arr.[idx1..idx2-1]), get (arr.[idx2..finIdx])

let solve =
    seq {
        for p in Util.permutations [1..9] Set.empty do
            for a = 1 to 4 do
                for b = a+1 to 5 do
                    let x,y,z = deconstruct p a b
                    if x * y = z then yield z
    } 
    |> Seq.distinct |> Seq.sum



                

