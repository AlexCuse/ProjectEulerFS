module ProjectEuler.Problem27

#light

let solve =
    let primes1k = 
        PrimeFinder.primes 1000L //should be enough
        |> Seq.map (int)
        |> List.ofSeq
    
    let isPrime = 
        let isPrimeFunc num =
            primes1k |> List.exists((=) num)
        Memoizer.memoize isPrimeFunc

    let mutable max = 0
    let mutable result = 0

    for a in -999..2..999 do
        for b in -999..2..999 do
            let mutable seqLength = 0
            let mutable num = 0
            while num = 0 || isPrime num do 
                num <- seqLength*seqLength + a*seqLength + b
                seqLength <- seqLength + 1
            
            if seqLength > max then
                max <- seqLength
                result <- a * b

    result