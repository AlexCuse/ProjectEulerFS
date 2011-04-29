#light
//need to load this dll to get at the hash set
//#r @"C:\Windows\assembly\GAC_MSIL\System.Core\3.5.0.0__b77a5c561934e089\System.Core.dll"
module ProjectEuler.PrimeFinder

open System
open System.Collections.Generic
let start = DateTime.Now;

let sieve (x:Int64) =
    let nonprimes = new HashSet<Int64>()//this will hold the sieve
    nonprimes.Add(Convert.ToInt64(1))|>ignore
    let mutable n = 2
    while n<=(int)(Math.Sqrt((double)x)) do
        for q in Convert.ToInt64(n + n) .. Convert.ToInt64(n) .. x do
            if not (nonprimes.Contains(q)) then
                nonprimes.Add(q)|>ignore //the error we get here
        done
        n<- n + 1 + (n%2)
    done
    nonprimes
 
let primes (x:Int64) = //get all primes up to x - can this be more sequency?
    let s = sieve x
    List.filter(fun n ->not (s.Contains(n))) [Convert.ToInt64(1)..x]

