#light
module ProjectEuler.Problem1

open System

let sumThreesFives=
    [1 .. 999]|>Seq.fold( fun ctr f-> 
        if f % 3 = 0 || f % 5 = 0 then 
            ctr + f 
            else ctr) 0
