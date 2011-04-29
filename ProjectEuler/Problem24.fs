module ProjectEuler.Problem24

#light

open System;

//not sure this will work but it provides a starting point
//http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let solve = 
    permutations [0..9] Set.empty
        |> Seq.skip(999999)
        |> Seq.head