module ProjectEuler.Problem26

// immutable, cleaned up
let brent f x = 
                   
    let rec applyUntilEqual func (power, lam, mu, tortoise, hare) =
        if (tortoise = hare) then
            (power, lam, mu, tortoise, hare)
        else
            applyUntilEqual func (func (power, lam, mu, tortoise, hare))

    let phaseOne (power, lam, mu, tortoise, hare) =
        if (power = lam) then
            (power * 2, 1, mu, hare, (f hare))
        else 
            (power, lam + 1, mu, tortoise, (f hare))

    let advanceHare (power, lam, mu, tortoise, hare) =
        let rec advance times h =
            if times = 1 then
                hare
            else 
                advance (times - 1) (f h)
        (power, lam, mu, tortoise, advance lam hare)

    let phaseTwo (power, lam, mu, tortoise, hare) =
        (power, lam, mu + 1, f tortoise, f hare)

    (1, 1, 0, x, f x) 
        |> applyUntilEqual phaseOne
        |> advanceHare
        |> applyUntilEqual phaseTwo
        |> fun (_, lam, mu, _, _) -> lam, mu

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