module ProjectEuler.Problem64

#light

open Cycle

let isSquare n =
    System.Math.Pow((float n), 0.5) % 1.0 = 0.0

let solve =
    let squares, nonsquares =
        [1..9999] |> List.partition isSquare        
    
    let mutator (n, a, m, d) =
        let nRt = int(System.Math.Sqrt(float n))
        let m' = d * a - m
        let d' = (n - m' * m') / d
        let a' = (nRt + m') / d'
        n, a', m', d'

    nonsquares
        |> List.map (fun ns -> brent mutator (ns, int(System.Math.Sqrt(float ns)), 0, 1))
        |> List.filter (fun (lam, _) -> lam &&& 1 = 1)
        |> List.length


//this method is MUCH faster, uses mutable state heavily though - still kind of a neat little function

//let continuedFraction n =
//    let sqrtN = int (System.Math.Sqrt(float n))
//    let a = ref sqrtN
//    let m = ref 0
//    let d = ref 1
//
//    seq {
//        //yuck!
//        m := !a * !d - !m
//        d := (n - !m * !m)/ !d
//        a := (!m + sqrtN) / !d
//        yield a
//        while not (!d = 1) do
//            m := !a * !d - !m
//            d := (n - !m * !m)/ !d
//            a := (!m + sqrtN) / !d
//            yield a
//    } |> List.ofSeq

//let solve =
//    let squares, nonsquares =
//        [1..9999] |> List.partition isSquare     

//    nonsquares 
//        |> List.map continuedFraction
//        |> List.filter (fun x -> x.Length &&& 1 = 1)
//        |> List.length
