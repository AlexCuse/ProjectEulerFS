module ProjectEuler.Problem61

#light
open Util

let cyclic a b =
    let a' = a |> digitsFrom |> Array.ofList
    let b' = b |> digitsFrom |> Array.ofList

    a'.[(a'.Length - 2)..(a'.Length - 1)] = b'.[0..1]

let combinations min max=
    let roots = Seq.unfold(fun n -> Some(n, n + 1)) 1

    let tw sq = 
        sq 
        |> Seq.skipWhile (fun n -> n < min)
        |> Seq.takeWhile (fun n -> n < max)

    let namedTuplize name v =
        name, v

    let tri = roots |> Seq.map triangle |> tw |> Seq.map (namedTuplize "tri") |> List.ofSeq //|> Seq.filter (fun n -> not (hex |> Seq.exists(fun n' -> n' = n))) 
    let sqr = roots |> Seq.map square |> tw |> Seq.map (namedTuplize "sqr") |> List.ofSeq
    let pnt = roots |> Seq.map pentagon |> tw |> Seq.map (namedTuplize "pnt") |> List.ofSeq
    let hex = roots |> Seq.map hexagon |> tw |> Seq.map (namedTuplize "hex") |> List.ofSeq
    let hpt = roots |> Seq.map heptagon |> tw |> Seq.map (namedTuplize "hpt") |> List.ofSeq
    let oct = roots |> Seq.map octagon |> tw |> Seq.map (namedTuplize "oct") |> List.ofSeq

    let all = List.concat [tri; sqr; pnt; hex; hpt; oct;]

    let findMatches n excludeList =
        all 
        |> List.filter (fun (nm, _) -> not(excludeList |> List.exists(fun x -> x = nm)))
        |> List.filter (fun (_, v) -> cyclic n v)
        
    seq {
        for v1 in all do
            for v2 in findMatches (snd v1) ([fst v1]) do
                for v3 in findMatches (snd v2) ([fst v1;fst v2]) do
                    for v4 in findMatches (snd v3) ([fst v1;fst v2; fst v3]) do
                        for v5 in findMatches (snd v4) ([fst v1;fst v2; fst v3; fst v4]) do
                            for v6 in findMatches (snd v5) ([fst v1;fst v2; fst v3; fst v4; fst v5]) do
                                if cyclic (snd v6) (snd v1) then yield [snd v1; snd v2; snd v3; snd v4; snd v5; snd v6]
                                //if cyclic (snd v6) (snd v1) then yield [v1;v2;v3;v4;v5;v6]
    }

let solve =
    (combinations 1000 10000)
    |> Seq.map (fun n -> (n |> List.sum), n)
    |> Seq.distinctBy (fun (sum, _) -> sum)
    |> Seq.head




