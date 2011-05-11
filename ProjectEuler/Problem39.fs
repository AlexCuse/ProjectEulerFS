module ProjectEuler.Problem39

#light

// can pythagorean triplets (only getting 2 vals but can be extended) in p9 help for this?

// a + b + c = p
// a * a + b * b = c * c
// a <= b < c

let possibleTriangles perimeter =
    // any side of an equilateral triangle will = p/3
    // c will always be opposite largest angle - longest leg, so  c > (p/3)
    // however for all sides of any triangle, c < (p / 2)
    // (p / 3) < c < (p / 2)
    [(3 * perimeter)/10 .. (5 * perimeter)/10] //only need integers
    |> List.filter (fun c ->
        [1..perimeter]
        |> Seq.takeWhile (fun x -> (x + c) < perimeter) //highest value that has remote possibility of matching 
        |> Seq.exists (fun b -> (pown (perimeter-b-c) 2 + pown b 2) = pown c 2)) // aa + bb == cc
    |> List.length

let solve = 
    [1..1000] 
    |> List.sortBy ( fun x -> -1 * possibleTriangles x)
    |> List.head