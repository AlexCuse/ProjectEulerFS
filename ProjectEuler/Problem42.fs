module ProjectEuler.Problem42

#light

let triangles =
    let rec getTriangles n start = 
        seq {   let tri = n + start
                yield tri
                yield! getTriangles (n + 1) tri }
    getTriangles 1 0

let wordNums =
    let intValue (word:string) =
        word.ToUpper().ToCharArray() |> Array.fold (fun acc x -> acc + (int x) - 64) 0
    seq {
        for wrd in System.IO.File.ReadAllText("words.txt").Split(',') do
            yield intValue (wrd.Replace("\"", ""))
    }

let solve = 
    let vals = wordNums |> List.ofSeq
    let mx = vals |> List.max
    let tris = triangles |> Seq.takeWhile (fun x -> x < mx) |> Set.ofSeq
    vals 
    |> Seq.filter tris.Contains
    |> Seq.length