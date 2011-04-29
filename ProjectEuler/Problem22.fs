module ProjectEuler.Problem22

#light
open System;
open System.IO;

let charScore (c:char) =
    (bigint (int c)) - (bigint 64)

let rec lettersScore name (startScore : bigint) = 
    match name with 
    | head :: tail -> lettersScore tail (startScore + (charScore head))
    | [] -> startScore

let nameScore (name:string) = 
    lettersScore (List.ofSeq(name)) (bigint 0)

let multipliedNameScore (name:string) (pos:bigint) =
    (nameScore name) * pos

//generic implementation - inlined below
let sumScores x =
    x 
    |> Seq.sort
    |> Seq.mapi(fun i x -> (i + 1, x))
    |> Seq.fold(fun sum name -> sum + (multipliedNameScore (snd name) (bigint (fst name))) ) (bigint 0)

let test =
    let lst = seq {
        yield "ALEX"
        yield "ALEX"
        //yield "ALEX"
    }

    sumScores lst

let solve = 
    File.ReadAllText("names.txt").Replace("\"", "").Split(',') 
        |> Seq.sort
        |> Seq.mapi(fun i x -> (i + 1, x.ToUpper()))//already upper case, but to be safe
        |> Seq.fold(fun sum name -> sum + (multipliedNameScore (snd name) (bigint (fst name))) ) (bigint 0)


