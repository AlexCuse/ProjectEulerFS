module ProjectEuler.Problem31

#light

let coins = [|1;2;5;10;20;50;100;200|]//pence

let rec changeCombinations total denominations =
    match total, denominations with 
    | 0, _ -> 1
    | _ when total < 0 -> 0
    | _ when denominations <= 0 && total >= 1 -> 0
    | _ -> (changeCombinations total (denominations - 1)) + (changeCombinations (total - coins.[denominations - 1]) denominations)


let solve =
    changeCombinations 200 coins.Length