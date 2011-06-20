module ProjectEuler.Problem67

#light

open System

let a = Array2D.zeroCreate<int> 100 100

System.IO.File.ReadAllLines("triangle.txt") |> Seq.iteri (fun idx line ->
    line.Split(' ') |> Seq.iteri (fun idx' vl -> a.[idx, idx'] <- (int vl)))

let costs = Array2D.zeroCreate<int> 100 100

Array2D.blit a 99 0 costs 99 0 1 100 |> ignore

let max a b = if a > b then a else b

//start in last row
for i in 98 .. -1 .. 0 do
    for j in 0 .. i do
        costs.[i,j] <- a.[i,j] + max costs.[i+1,j] costs.[i+1, j+1]

let solve =
    //this array position will contain maximum possible sum accumulated when traversing to bottom
    costs.[0,0]