module ProjectEuler.Problem61

#light
open Util

let formulated min max =
    let roots = Seq.unfold(fun n -> Some(n, n + 1)) 1

    let tw sq = 
        sq 
        |> Seq.skipWhile (fun n -> n < min)
        |> Seq.takeWhile (fun n -> n < max)

    let tuplize name v = name, v

    let tri = roots |> Seq.map triangle |> tw |> Seq.map (tuplize "tri") |> List.ofSeq
    let sqr = roots |> Seq.map square |> tw |> Seq.map (tuplize "sqr") |> List.ofSeq
    let pnt = roots |> Seq.map pentagon |> tw |> Seq.map (tuplize "pnt") |> List.ofSeq
    let hex = roots |> Seq.map hexagon |> tw |> Seq.map (tuplize "hex") |> List.ofSeq
    let hpt = roots |> Seq.map heptagon |> tw |> Seq.map (tuplize "hpt") |> List.ofSeq
    let oct = roots |> Seq.map octagon |> tw |> Seq.map (tuplize "oct") |> List.ofSeq

    List.concat [tri; sqr; pnt; hex; hpt; oct;]

let cyclic a b =
    let a' = a |> digitsFrom |> Array.ofList
    let b' = b |> digitsFrom |> Array.ofList

    a'.[(a'.Length - 2)..(a'.Length - 1)] = b'.[0..1]

let answers candidates =
    let findMatches n excludeList =
        candidates 
        |> List.filter (fun (nm, _) -> not(excludeList |> List.exists(fun x -> x = nm)))
        |> List.filter (fun (_, v) -> not (n = v) && cyclic n v)// check equality for situation where same number drawn from hex & tri, etc...
        
    seq {
        for v1 in candidates do //one instance of cycle will always start with triangle num, and there are the most of these so could start with triangles (it is not much faster though)
            for v2 in findMatches (snd v1) ([fst v1]) do
                for v3 in findMatches (snd v2) ([fst v1;fst v2]) do
                    for v4 in findMatches (snd v3) ([fst v1;fst v2; fst v3]) do
                        for v5 in findMatches (snd v4) ([fst v1;fst v2; fst v3; fst v4]) do
                            for v6 in findMatches (snd v5) ([fst v1;fst v2; fst v3; fst v4; fst v5]) do
                                if cyclic (snd v6) (snd v1) then yield [snd v1; snd v2; snd v3; snd v4; snd v5; snd v6]
                                //if cyclic (snd v6) (snd v1) then yield [v1;v2;v3;v4;v5;v6]
    }

let solve =
    answers (formulated 1000 10000)
    |> Seq.map (fun n -> (n |> List.sum), n)
    |> Seq.head




