module ProjectEuler.Cycle

#light

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