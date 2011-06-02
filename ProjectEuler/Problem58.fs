module ProjectEuler.Problem58

#light
(*

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

let calcRing n =
    let nSquared = n * n 
    nSquared + (nSquared - (n - 1)) + (nSquared - (2 * (n - 1))) + (nSquared - (3 * (n - 1)))

*)

let isPrime n =
    let rec checkRemaining n r f =
        if f >= r then true
        elif n % f = 0L then false
        elif n % (f + 2L) = 0L then false
        else checkRemaining n r (f + 6L)
        
    match n with 
    | 1L -> false
    | _ when n < 4L -> true
    | _ when n % 2L = 0L -> false
    | _ when n < 9L -> true
    | _ when n % 3L = 0L -> false
    | _ ->
        let r = System.Convert.ToInt64(System.Math.Floor(System.Math.Sqrt(float n)))
        //let r = (float n) |> sqrt |> floor |> int64
        checkRemaining n r 5L

let ringSizes =
    Seq.unfold (fun n -> Some(n, n+2)) 1

let corners n =
    //0 to 3 for each corner
    let positionCalc n v =
        (n * n) - (v * (n - 1))
    [3..-1..0] |> List.map (fun c -> positionCalc n c)

let solve =
    ringSizes
    |> Seq.zip (ringSizes |> Seq.map corners)
    |> Seq.scan (fun (pcount, sz) (x, y)-> 
        pcount + (x |> List.fold(fun acc n -> if (isPrime (int64 n)) then acc + 1 else acc ) 0)
        , sz + (if sz = 0 then 1 else 4)) (0,0)
    |> Seq.skipWhile (fun (pcount, sz) -> pcount = 0 || ((float pcount) / (float sz)) > 0.1)
    |> Seq.head