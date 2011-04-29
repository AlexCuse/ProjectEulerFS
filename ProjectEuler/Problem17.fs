module ProjectEuler.Problem17

#light
open System;

let nums = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let tens = [| "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]

let numberAsString num = 
    let thousandsDigit =
        if num > 999 then
            let numAsString = num.ToString()
            let digit = Int32.Parse(numAsString.Substring(numAsString.Length - 4, 1))
            (Array.get nums digit) + "thousand"
        else ""
    let hundredsDigit =
        let hundredsString dgt = if dgt = 0 then "" else "hundred"

        if num > 99 then
            let numAsString = num.ToString()
            let digit = Int32.Parse(numAsString.Substring(numAsString.Length - 3, 1))
            (Array.get nums digit)  + (hundredsString digit)
        else ""

    let lastTwoDigits = 
        let numAsString = num.ToString()
        let lastTwo = 
            match numAsString.Length with
            | 1 -> numAsString
            | _ -> numAsString.Substring(numAsString.Length - 2, 2)

        let onesPlace = Int32.Parse(numAsString.Substring(numAsString.Length - 1, 1));

        if lastTwo.Length > 1 then
            let tensPlace = Int32.Parse(numAsString.Substring(numAsString.Length - 2, 1))
            let digit = Int32.Parse(lastTwo)
            if digit < 20 then
                Array.get nums digit
            else
                (Array.get tens (tensPlace - 2)) + (Array.get nums onesPlace)
        else
            Array.get nums onesPlace

    let andString = 
        if num > 100 then
            if num % 100 > 0 then "and" else ""
        else ""

    thousandsDigit + hundredsDigit + andString + lastTwoDigits;

let rec numbersString num limit str =
    if num <= limit then
         numbersString (num + 1) limit str + (numberAsString num)
    else str   

let rec recursiveList lst func =
    match lst with
    | [] -> ""
    | hd::tail -> func hd + recursiveList tail func 

let numbers min max =
    seq {
        for i in min..max do yield i
    }

let solve =
    //let result = (numbersString 1 1000 "")
    //let result = recursiveList [1..1000] numberAsString
    //let result = [1..1000]|> List.fold(fun acc f -> acc + (numberAsString f)) ""
    let result = numbers 1 1000 |> Seq.fold(fun acc f -> acc + (numberAsString f)) ""
    result.Length