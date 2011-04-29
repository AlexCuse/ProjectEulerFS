module ProjectEuler.Problem25

#light

let fibs = Seq.unfold (fun (n0, n1) -> Some(n0, (n1, n0 + n1))) (1I,1I)

let solve = 
    let length =
        fibs
            |> Seq.takeWhile((>) (pown 10I 999)) 
            |> Seq.length

    length + 1

(*
    let length = 
        fibs 
            |> Seq.takeWhile (fun fib -> fib.ToString().Length < 1000)
            |> Seq.length 

 to see actual number   
        |> Seq.find(fun fib -> fib.ToString().Length = 1000)

*)