module ProjectEuler.Problem15
(*

Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?

see Pascal's Triangle (http://en.wikipedia.org/wiki/Pascal%27s_triangle)

only need to calculate 'center' value in row ie

1  1  1  1
1  2  3  4
1  3  6  10
1  4  10  20

values we are concerned with form diagonal from top left to bottom right.  

Will want product of this diagonal

Each one of these values will represent 2 rows of the triangle (consider the same square rotated 45 degrees clockwise, 2 is on the 3rd row

*)
#light

let rec pascalRoutes nextRow column =
    match column with
    | 0L -> 1L
    | _ -> (nextRow - column) * (pascalRoutes nextRow (column - 1L))/ column

let numberOfRoutesFor gridSize =
    pascalRoutes (int64(2* gridSize + 1)) (int64 gridSize)

let solve =
    numberOfRoutesFor 20

