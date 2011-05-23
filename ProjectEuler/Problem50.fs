module ProjectEuler.Problem50

#light

let primes =
    PrimeFinder.primes 999999L
    |> List.map int
    |> Seq.ofList

let primeSeq n =
    primes 
    |> Seq.filter (fun x -> x > n)
    |> Seq.scan (fun (sum, count) x -> (sum+x, count+1)) (n, 1)
    |> Seq.takeWhile (fun (sum, count) -> sum < 1000000)
    |> Seq.filter (fun (sum, count) -> Util.isPrime sum)
    |> Seq.maxBy (fun (sum, count) -> count)

let solve =
    primes |> Seq.map primeSeq |> Seq.maxBy (fun (sum, count) -> count)