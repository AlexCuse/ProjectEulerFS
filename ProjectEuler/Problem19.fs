#light
module ProjectEuler.Problem19

open System

//Thirty days has September,
//April, June and November.
//All the rest have thirty-one,
//Saving February alone,
//Which has twenty-eight, rain or shine.
let monthdays = 
    [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

//And on leap years, twenty-nine.
//A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
let leapIncr (year:int, month:int) =
    if month <> 1 then 0 else
        if (year % 4 = 0 && year % 100 > 0) || (year % 400 = 0) then 1 else 0

let rec nextFirst (currMonth:int) (currYear:int) (currWeekday:int) =
    seq{    let nextWeekday = (currWeekday + (monthdays.[currMonth] + leapIncr(currYear, currMonth))) % 7
            let nextYear = currYear + (if currMonth = 12 then 1 else 0)
            let nextMonth = (currMonth + 1) % 12
            yield nextWeekday
            yield! nextFirst nextMonth nextYear nextWeekday }

let solve =
   let startWeekday = 
        nextFirst 0 1900 1
            |>Seq.skip(11)
            |>Seq.head //First need to compute the weekday for 1/1/1901

   nextFirst 0 1901 startWeekday
        |>Seq.take(1200)
        |>Seq.fold(fun ctr f -> if f = 0 then ctr + 1 else ctr)0