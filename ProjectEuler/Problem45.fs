module ProjectEuler.Problem45

#light

open LongUtil

// every hexagonal number is also a triangular number
// seems that a long is needed for isPentagon check, though result appears to fit in int32
let candidates = 
    Seq.unfold(fun n -> Some(n, n + 1L)) 144L //looking for something past hex 143
    |> Seq.map hexagon

let solve =
    candidates
    |> Seq.find isPentagon