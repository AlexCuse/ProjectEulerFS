module ProjectEuler.Problem23

#light

let factors x = Util.factors x

let isAbundant x =
    let sum = factors x |> Seq.fold (+) 0
    (sum - x > x)

let abundantNumberCheck = [| for x in 0 .. 28123 -> isAbundant x |]

let abundantList = [1 .. 28123] |> List.filter (fun x -> isAbundant x)

let rec isPairOfAbundants list x =
    if (List.head list) > x then false
    elif abundantNumberCheck.[x - (list |> List.head)] then true
    else isPairOfAbundants (list |> List.tail) x       

let nonPairOfAbundants = 
    [1 .. 28123] 
        |> List.filter (fun num -> not(isPairOfAbundants abundantList num)) 

    // Return their sum
let solve = 
    nonPairOfAbundants |> List.fold (+) 0 
