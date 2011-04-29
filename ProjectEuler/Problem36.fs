module ProjectEuler.Problem36

#light
open System;

let getBinDigits n =
    //n &&& 1 = rightmost bit
    //n >>> 1 = shift bits over 1 (dropping rightmost)
    let rec f n acc =
        if n = 0 then acc
                 else f (n >>> 1) ((n &&& 1) :: acc)
    f n [] 
        
let getBinaryString n =
    getBinDigits n
        |> Seq.fold(fun acc x -> acc + x.ToString()) ""

let reverseString (s:string) = 
    new string(Array.rev (s.ToCharArray()))

let decimalAndBinaryPalindrome num =
    let str = num.ToString()
    if str = (reverseString str) then
        let binStr = getBinaryString num
        binStr = (reverseString binStr)
    else
        false

let solve =
    [1..1000000]
        |>Seq.filter(decimalAndBinaryPalindrome)
        |>Seq.sum
