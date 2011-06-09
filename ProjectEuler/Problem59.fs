module ProjectEuler.Problem59

#light
open System
open System.IO
open Combinations
open Util

let encryptedChars = 
    File.ReadAllText("cipher1.txt").Split(',') 
    |> Array.map int

let words =
    File.ReadAllText("words.txt").Split(',')
    |> Array.map (fun x -> x.Replace("\"", ""))

let potentialKeys =
    let arr = [|97..122|]
    let combo = ref (new Combination(26, 3))
    seq {
        yield (!combo).ApplyTo(arr)
        while not((!combo).IsLast()) do
            combo := (!combo).Successor()
            yield (!combo).ApplyTo(arr) 
    }
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

        words |> Array.fold (fun acc x -> if (str.Contains x) then acc + 1 else acc) 0, str
    )
    |> Seq.maxBy(fun (k, v) -> k)

