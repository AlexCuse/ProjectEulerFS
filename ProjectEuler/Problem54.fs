module ProjectEuler.Problem54

open System.IO

type value =
    | CardValue of int | Jack | Queen | King | Ace

type suit = 
    | Club | Diamond | Heart | Spade

type card = { value : value; suit : suit }

let valueRanks = 
    [|CardValue 2;CardValue 3;CardValue 4;CardValue 5;CardValue 6;CardValue 7;CardValue 8;CardValue 9;CardValue 10;Jack;Queen;King;Ace|]

let valueRank value =
    valueRanks |> Array.findIndex (fun v -> v = value)

let suitRanks =
    [|Club; Diamond; Heart; Spade|]

let suitRank value =
    suitRanks |> Array.findIndex (fun v -> v = value)

let getCard (sr: string) =
    let s = sr.Substring(1)
    let v = sr.Substring(0, 1)
    let va = 
        match v with
        | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> CardValue (int v)
        | "T" -> CardValue (10)
        | "J" -> Jack
        | "Q" -> Queen
        | "K" -> King
        | "A" -> Ace
        | _ -> failwith ("Invalid card value: " + v)

    let su = 
        match s with
        | "S" -> Spade
        | "H" -> Heart
        | "D" -> Diamond 
        | "C" -> Club
        | _ -> failwith ("Invalid suit: " + s)
    
    { card.value = va; card.suit = su }

type handRank =
    | HighCard of card
    | Pair of card
    | TwoPairs of card * card
    | Three of card
    | Straight of card 
    | Flush
    | FullHouse of card * card
    | Four of card
    | StraightFlush of card 
    | RoyalFlush

//relies on some functions above
type hand (cards:card[]) =
    member x.Cards = cards
    member x.HighCard = cards |> Array.maxBy (fun crd -> valueRank crd.value)
    member x.Groups = 
        cards 
            |> Seq.groupBy (fun crd -> crd.value)
            |> Seq.filter (fun (key, grp) -> grp |> Seq.length > 1)
    member x.Straight =
        cards 
            |> Array.sortBy (fun crd -> crd.value)
            |> Seq.windowed 2
            |> Seq.forall (fun [|c1;c2|] -> valueRank c1.value = (valueRank c2.value) - 1)
    member x.Flush = 
        cards 
            |> Seq.groupBy (fun crd -> crd.suit)
            |> Seq.length = 1
    //clean up
    member x.Value =
        if x.Straight && x.Flush then
            if x.HighCard.value = Ace then RoyalFlush else StraightFlush x.HighCard
        elif x.Groups |> Seq.length = 1 && x.Groups |> Seq.head |> (fun (_, s) -> s |> Seq.length) = 4 then
            Four (x.Groups |> Seq.head |> (fun (_, s) -> s |> Seq.maxBy (fun crd -> suitRank crd.suit)))
        elif x.Groups |> Seq.length = 2 && x.Groups |> Seq.collect (fun (_, s) -> s) |> Seq.length = 5 then
            let threeOf = x.Groups |> Seq.map (fun (_, s) -> s) |> Seq.filter (fun s -> s |> Seq.length = 3) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit)
            let twoOf = x.Groups |> Seq.map (fun (_, s) -> s) |> Seq.filter (fun s -> s |> Seq.length = 2) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit)
            FullHouse(threeOf, twoOf)
        elif x.Flush then Flush
        elif x.Straight then Straight x.HighCard
        elif x.Groups |> Seq.length = 1 && x.Groups |> Seq.map (fun (_, s) -> s) |> Seq.head |> Seq.length = 3 then 
            Three (x.Groups |> Seq.map (fun (_, s) -> s) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit))
        elif x.Groups |> Seq.length = 2 &&  x.Groups |> Seq.collect (fun (_, s) -> s) |> Seq.length = 4 then
            let high = x.Groups |> Seq.sortBy (fun (k, _) -> valueRank k) |> Seq.map (fun (_, s) -> s) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit)
            let low = x.Groups |> Seq.sortBy (fun (k, _) -> -1 * valueRank k) |> Seq.map (fun (_, s) -> s) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit)
            TwoPairs(high, low)
        elif x.Groups |> Seq.length = 1 then
            Pair (x.Groups |> Seq.map (fun (_, s) -> s) |> Seq.head |> Seq.maxBy (fun crd -> suitRank crd.suit))
        else
            HighCard (x.HighCard)

let compareHands (left:hand) (right:hand) =
    let leftRank, rightRank = left.Value, right.Value
    if leftRank > rightRank then "L"
    elif rightRank > leftRank then "R"
    else
        if (left.HighCard.value |> valueRank) > (right.HighCard.value |> valueRank) then "L"
        elif (left.HighCard.value |> valueRank) > (right.HighCard.value |> valueRank) then "R"
        else
            if (left.HighCard.suit |> suitRank) > (right.HighCard.suit |> suitRank) then "L" else "R"

let solve = 
    File.ReadAllLines "poker.txt"
    |> Seq.map (fun ln -> ln.Split(' '))
    |> Seq.map (fun ln -> ln.[0..4], ln.[5..9])
    |> Seq.map (fun (p1, p2) -> (p1 |> Array.map (fun sv -> getCard sv), (p2 |> Array.map (fun sv -> getCard sv))))
    |> Seq.map (fun (p1, p2) -> hand p1, hand p2)
    |> Seq.filter (fun (p1, p2) -> compareHands p1 p2 = "L")
    |> Seq.length
        