module ProjectEuler.Problem44

#light

open System.Collections.Generic

let pentagon n =
    (n * ((3 * n) - 1)) / 2

let pentagonCache = 
    let innerCache = new Dictionary<int, int>()
    [1..5000]
    |> List.iter (fun n -> innerCache.[n] <- pentagon n) 
    innerCache

let isPentagon x =
    let isPentagonFunc n = pentagonCache.ContainsValue n
    Memoizer.memoize isPentagonFunc x

let isValidPair (x, y) =
    let k = pentagonCache.[x]
    let j = pentagonCache.[y]
    isPentagon (j + k) && isPentagon (k - j)

let solve =
    [1..5000]
    |> Seq.collect (fun k -> [k-1..-1..1] |> List.map (fun j -> k, j))
    |> Seq.filter isValidPair
    |> Seq.head
    |> fun (k, j) -> pentagon k - pentagon j
