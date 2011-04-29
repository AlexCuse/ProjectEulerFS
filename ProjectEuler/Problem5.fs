#light
module ProjectEuler.Problem5
//open Math.BigInt

// Greater Common Divisor
let rec gcd a b =
 match b with
 | b when b = 0I -> a
 | b -> gcd b (a % b)

// Least Common Multiple
let lcm a b = a * b / (gcd a b)

let answer = Seq.reduce lcm [1I .. 20I]


(*

#light
open System

//brute force is too slow

let nums = 
    2000|>Seq.unfold(fun x -> Some(x, x + 20))
    |> Seq.filter (fun x -> 
        let check q =
            let ret = ref true
            for n in [3 .. 19] do
                if x % n > 0 then
                    ret := false
            !ret    
        check x    
    )
    |> Seq.min


let solve =
    nums
    
    *)
