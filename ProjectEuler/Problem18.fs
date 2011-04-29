module ProjectEuler.Problem18

#light

//need a 2d array 15 * 15 to hold all values
let a = Array2D.zeroCreate<int> 15 15

a.[0,0] <- 75
a.[1,0] <- 95
a.[1,1] <- 64
a.[2,0] <- 17
a.[2,1] <- 47
a.[2,2] <- 82
a.[3,0] <- 18
a.[3,1] <- 35
a.[3,2] <- 87
a.[3,3] <- 10
a.[4,0] <- 20
a.[4,1] <- 04
a.[4,2] <- 82
a.[4,3] <- 47
a.[4,4] <- 65
a.[5,0] <- 19
a.[5,1] <- 01
a.[5,2] <- 23
a.[5,3] <- 75
a.[5,4] <- 03
a.[5,5] <- 34
a.[6,0] <- 88
a.[6,1] <- 02
a.[6,2] <- 77
a.[6,3] <- 73
a.[6,4] <- 07
a.[6,5] <- 63
a.[6,6] <- 67
a.[7,0] <- 99
a.[7,1] <- 65
a.[7,2] <- 04
a.[7,3] <- 28
a.[7,4] <- 06
a.[7,5] <- 16
a.[7,6] <- 70
a.[7,7] <- 92
a.[8,0] <- 41
a.[8,1] <- 41
a.[8,2] <- 26
a.[8,3] <- 56
a.[8,4] <- 83
a.[8,5] <- 40
a.[8,6] <- 80
a.[8,7] <- 70
a.[8,8] <- 33
a.[9,0] <- 41
a.[9,1] <- 48
a.[9,2] <- 72
a.[9,3] <- 33
a.[9,4] <- 47
a.[9,5] <- 32
a.[9,6] <- 37
a.[9,7] <- 16
a.[9,8] <- 94
a.[9,9] <- 29
a.[10,0] <- 53
a.[10,1] <- 71
a.[10,2] <- 44
a.[10,3] <- 65
a.[10,4] <- 25
a.[10,5] <- 43
a.[10,6] <- 91
a.[10,7] <- 52
a.[10,8] <- 97
a.[10,9] <- 51
a.[10,10] <- 14
a.[11,0] <- 70
a.[11,1] <- 11
a.[11,2] <- 33
a.[11,3] <- 28
a.[11,4] <- 77
a.[11,5] <- 73
a.[11,6] <- 17
a.[11,7] <- 78
a.[11,8] <- 39
a.[11,9] <- 68
a.[11,10] <- 17
a.[11,11] <- 57
a.[12,0] <- 91
a.[12,1] <- 71
a.[12,2] <- 52
a.[12,3] <- 38
a.[12,4] <- 17
a.[12,5] <- 14
a.[12,6] <- 91
a.[12,7] <- 43
a.[12,8] <- 58
a.[12,9] <- 50
a.[12,10] <- 27
a.[12,11] <- 29
a.[12,12] <- 48
a.[13,0] <- 63
a.[13,1] <- 66
a.[13,2] <- 04
a.[13,3] <- 68
a.[13,4] <- 89
a.[13,5] <- 53
a.[13,6] <- 67
a.[13,7] <- 30
a.[13,8] <- 73
a.[13,9] <- 16
a.[13,10] <- 69
a.[13,11] <- 87
a.[13,12] <- 40
a.[13,13] <- 31
a.[14,0] <- 04
a.[14,1] <- 62
a.[14,2] <- 98
a.[14,3] <- 27
a.[14,4] <- 23
a.[14,5] <- 09
a.[14,6] <- 70
a.[14,7] <- 98
a.[14,8] <- 73
a.[14,9] <- 93
a.[14,10] <- 38
a.[14,11] <- 53
a.[14,12] <- 60
a.[14,13] <- 04
a.[14,14] <- 23

//another array to represent the "cost" in terms of maximum sum accumulated by moving through a given space
let costs = Array2D.zeroCreate<int> 15 15

// costs for bottom row is the same as bottom row values
Array2D.blit a 14 0 costs 14 0 1 15 |> ignore

let max a b = if a > b then a else b

//start in last row
for i in 13 .. -1 .. 0 do
    //i will also be appropriate #of columns
    for j in 0 .. i do
        //calculate cost for each space based on max of lower 2 costs + current value
        (* example:

        3
        |\
        5  12

        5 and 12 are unevaluated "costs"

        3 is an unprocessed row value

        cost for current position will be 
        (max of 5 or 12) + 3

        or 15
        *)
        costs.[i,j] <- a.[i,j] + max costs.[i+1,j] costs.[i+1, j+1]

let solve =
    //this array position will contain maximum possible sum accumulated when traversing to bottom
    costs.[0,0]
