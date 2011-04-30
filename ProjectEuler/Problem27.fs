module ProjectEuler.Problem27

#light

let solve =
    let primes100k = 
        PrimeFinder.primes 100000L //should be enough
        |> Seq.map (fun x -> int x)
        |> List.ofSeq
       
    let isPrime x =
        primes100k |>
        Seq.exists((=) x)

    let mutable max = 0
    let mutable result = 0

    for a in -1000..1..1000 do
        for b in -1000..1..1000 do
            let mutable seqLength = 0
            let mutable num = 0
            while num = 0 || isPrime num do 
                num <- seqLength*seqLength + a*seqLength + b
                seqLength <- seqLength + 1
            
            if seqLength > max then
                max <- seqLength
                result <- a * b

    result