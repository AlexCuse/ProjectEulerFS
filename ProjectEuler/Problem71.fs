module ProjectEuler.Problem71

#light

//http://en.wikipedia.org/wiki/Farey_sequence  (neighbors)

let solve = 
    let mutable a, c = 2, 5
    let mutable b, d = 3, 7

    while (c + d) <= 1000000 do
        a <- a + b
        c <- c + d

    a, c //numerator, denominator
