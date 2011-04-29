#light
module ProjectEuler.Problem4

open System
open System.Collections.Generic

let allProducts =
    (100, 100)|>Seq.unfold (fun (x,y) ->
        match y with
        | y when y < 0 -> None
        | y when x <= 999 ->
            if (y <= 999) then
                Some(x*y, (x, y+1))
            elif (x+1) <= 999 then
                Some( (x+1)*(x+1), (x+1, x+2) )
            else
                None
        | y -> None)
        
let solve =
    let rev (s:string) =
           let chunks =s.ToCharArray()
           let reversed = Array.rev chunks
           let r = new string(reversed)
           r
    allProducts|>Seq.filter(fun n ->
        n.ToString() = rev ( n.ToString() ) )
        |>Seq.max