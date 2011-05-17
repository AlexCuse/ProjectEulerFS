module ProjectEuler.Problem47

#light

open Util

let primes = PrimeFinder.primes 200000L |> List.map int |> Set.ofList

let nums = 
    Seq.unfold (fun n -> Some(n, n + 1)) 1

let filter n =
    factors n
    |> Seq.filter (fun n -> primes.Contains n) 
    //TODO: isPrime method in Util gives bad results, around 40k...
    //tom's ruby function
    //    def Utils.is_prime? n
    //        !(2..n - 1).any?{|x| n % x == 0} && n != 1
    //    end
    |> Seq.distinct
    |> Seq.length >= 4

let solve =
    nums
    |> Seq.filter filter
    |> Seq.windowed 4
    |> Seq.filter (fun n -> Seq.max n - Seq.min n = 3)
    |> Seq.head