module ProjectEuler.Problem59

#light
open System
open System.IO

let encryptedChars = 
    File.ReadAllText("cipher1.txt").Split(',') 
    |> Array.map int

let words =
    File.ReadAllText("words.txt").Split(',')
    |> Array.map (fun x -> x.Replace("\"", ""))
    |> Set.ofArray

let potentialKeys =
    seq {
        for c1 in ['a'..'z'] do
            for c2 in ['a'..'z'] do
                for c3 in ['a'..'z'] do
                    yield [|c1;c2;c3;|] |> Array.map int
    }
    |> Seq.distinct
    |> List.ofSeq

let rec triplets (arr:int[]) =
    seq {
        if (arr.Length <= 3) then
            yield arr.[0..(arr.Length - 1)]
        else 
            yield arr.[0..2]
            yield! triplets arr.[3..] 
    }

let decode (trip:int[]) (key:int[]) =
    seq {
        for i in 0..(trip.Length - 1) do
            yield char(trip.[i] ^^^ key.[i])
    }

let solve = 
    potentialKeys 
    |> List.map (fun key ->
        let decryptedChars = 
            encryptedChars 
            |> triplets
            |> Seq.map (fun t -> decode t key)
            |> Seq.collect (fun s -> s)
            |> Array.ofSeq

        let str = new String(decryptedChars)

        str.Split(' ') |> Array.fold (fun acc x -> if (words.Contains (x.ToUpper())) then acc + 1 else acc) 0, key, str
    )
    |> Seq.maxBy(fun (c, _, _) -> c)
    |> fun (_, key, v) -> v
    |> Seq.fold (fun acc c -> acc + (int c)) 0

