module ProjectEuler.Memoizer

#light
open System.Collections.Generic

let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let found, res = cache.TryGetValue(x)
        if found then res
        else
            let res = f x
            cache.[x] <- res
            res