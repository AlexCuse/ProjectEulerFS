module ProjectEuler.Problem26

#light

open Cycle

let findNumberGeneratingLongestRepeatingSequenceImmutable list =
    let rec calc lst func longestCycleYet numberYieldingCycle =
        match lst with
            | head :: tail 
                -> 
                    let tp = func head
                    if (max longestCycleYet (fst tp)) = longestCycleYet then
                        calc tail func longestCycleYet numberYieldingCycle
                    else 
                        calc tail func (fst tp) head
            | [] -> numberYieldingCycle

    let f (num, value, rem) = 
        (num, rem / num, ((rem % num) * 10))
        
    calc list (fun num -> brent f (num, 0, 1)) 0 0 

let solve = 
    findNumberGeneratingLongestRepeatingSequenceImmutable [1..1000]//can limit to primes for larger volumes, at least for this problem