module ProjectEuler.Problem21

#light
open System;
open System.Collections;

let generateFactors number =
    seq {
        yield 1L
        let max = 
            if number % 2L = 0L then number / 2L
            elif number % 3L = 0L then number / 3L 
            else number - 1L
        for i in 2L..max do
            if number % i = 0L then 
                yield i
    }

let sumDivisors root = 
    generateFactors root |> Seq.sum

let amicableComplement a = 
    let b = sumDivisors a
    let dB = sumDivisors b

    if (dB = a) && (b <> a) then
        b
    else 
        0L

let rec amicableSum root check acc (skip:BitArray) = 
    if check >= root then acc else
    let nextCheck = amicableComplement check
    if not (skip.Get((int)check)) then
        if nextCheck > 0L then
            if nextCheck < root then skip.Set((int)nextCheck, true)
            nextCheck + check + (amicableSum root (check + 1L) acc skip)
        else amicableSum root (check + 1L) acc skip
    else amicableSum root (check + 1L) acc skip
            
let solve =
    amicableSum 10000L 1L 0L (new BitArray(10000))
    