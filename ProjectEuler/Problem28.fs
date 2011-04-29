module ProjectEuler.Problem28

(*

example spiral:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

note that top right corner of spiral with sides of length N is N * N  (from center: 1, 9, 25)

other corners can be computed by subtracting (N - 1)
for example, in 5 * 5 band starting w/ 25 in top right we have (21, 17, 13)
                3 * 3 we have (7, 5, 3)

for each additional ring, total width will increase by two

we can start at outer ring since we know width (1001)

*)

(*
Calculating value for a ring :

let calcRing n =
    let nSquared = n * n 
    nSquared + (nSquared - (n - 1)) + (nSquared - (2 * (n - 1))) + (nSquared - (3 * (n - 1)))

Can be simplified to :
    (4 * nSquared) - (6 * (n - 1))

And further to :
    (4 * n * n) - (6 * n) + 6

At which point it is pretty easy to inline

*)

let spiralSum n =
    let rec spiralDiagonalSum n sum =
        match n with
            | 1 -> sum + 1
            | _ -> spiralDiagonalSum (n - 2) (sum + ((4 * n * n) - (6 * n) + 6))
    spiralDiagonalSum n 0

let solve = spiralSum 1001