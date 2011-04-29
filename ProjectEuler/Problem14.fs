module ProjectEuler.Problem14

#light
open System
open System.Linq

//The following iterative sequence is defined for the set of positive integers:
//
//n → n/2 (n is even)
//n → 3n + 1 (n is odd)
//
//Using the rule above and starting with 13, we generate the following sequence:
//13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//
//Which starting number, under one million, produces the longest chain?
//
//NOTE: Once the chain starts the terms are allowed to go above one million.

//let is_even x =
//    x % 2L = 0L
//
//let rec collatz n (partial:int64 list) =
//    match n with
//        | 1L -> partial @ [1L]
//        | x when is_even x -> collatz (x/2L) (partial @ [n])
//        | _ -> collatz ((3L*n) + 1L) (partial @ [n])
//
//let calculate_sequence n =
//    collatz n [] 
//
//let solve =
//    let sequences = [for i in 1L..1000000L -> calculate_sequence i]
//    let lengths = List.map List.length sequences
//    let max = List.max lengths
//    let max_index = List.findIndex (fun (x:int64 list) -> x.Length = max) sequences
//    max_index + 1


//http://stefanoricciardi.com/2010/11/23/project-euler-problem-14-in-f/
open System.Collections.Generic

let is_odd x =
    x % 2L = 1L

let rec collatz_sequence n (partial:int64 list) (cache: IDictionary<int64, int64 list>) =
    if cache.ContainsKey(n) then
        let result = partial @ cache.[n]
        let sequence_starter = List.head result
        if not(cache.ContainsKey(sequence_starter)) then
            cache.Add(sequence_starter, result) |> ignore
        result
    else
        match n with
        | 1L ->
            let result = partial @ [1L]
            let sequence_starter = List.head result
            cache.Add(sequence_starter, result) |> ignore
            result
        | n when is_odd n -> collatz_sequence ((3L*n) + 1L) (partial @ [n]) cache
        | _ -> collatz_sequence (n/2L) (partial @ [n]) cache

let sequence_cache = new Dictionary<int64, int64 list>()

let calculate_sequence n =
    collatz_sequence n [] sequence_cache

let max_sequence_lengths (max:int64) =
    let all_sequences = [for i in 1L..max -> calculate_sequence i]
    let lengths = List.map List.length all_sequences
    let max = List.max lengths
    let max_index = List.findIndex (fun (x:int64 list) -> x.Length = max) all_sequences
    max_index + 1

let solve = 
    max_sequence_lengths 1000000L
