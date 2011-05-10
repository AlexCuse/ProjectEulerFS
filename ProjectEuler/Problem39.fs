module ProjectEuler.Problem39

#light

// can pythagorean triplets (only getting 2 vals but can be extended) in p9 help for this?

// a * a + b * b = c * c
// a + b + c = p
// p < 1000
// a < b < c ?

let possibleTriangles perimeter =
    [(4 * perimeter)/10 .. (6 * perimeter)/10]
    |> List.filter (fun c ->
        [1..perimeter]
        |> Seq.takeWhile (fun x -> (x + c) < perimeter) // check could be smarter, but this rules out extreme impossibilities
        |> Seq.exists (fun b -> (pown (perimeter-b-c) 2 + pown b 2) = pown c 2)) // aa + bb == cc
    |> List.length

let solve = 
    [1..1000] 
    |> List.sortBy ( fun x -> -1 * possibleTriangles x)
    |> List.head