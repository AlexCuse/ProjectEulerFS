module ProjectEuler.Problem64

#light

let isSquare n =
    System.Math.Pow((float n), 0.5) % 1.0 = 0.0

let continuedFraction (n:int) =
    let sqrtN = int (System.Math.Sqrt(float n))
    let a = ref sqrtN
    let p = ref 0
    let q = ref 1

    seq {
        //yuck!
        p := !a * !q - !p
        q := (n - !p * !p)/ !q
        a := (!p + sqrtN) / !q
        yield a
        while not (!q = 1) do
            p := !a * !q - !p
            q := (n - !p * !p)/ !q
            a := (!p + sqrtN) / !q
            yield a
    } |> List.ofSeq

let solve =
    let squares, nonsquares =
        [1..9999] |> List.partition isSquare        
               
    nonsquares 
        |> List.map continuedFraction
        |> List.filter (fun x -> x.Length &&& 1 = 1)
        |> List.length
