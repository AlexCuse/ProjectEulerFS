#light
module ProjectEuler.Program
open System;
open System.Diagnostics;
open System.Reflection;

let dynamicExecute prob =
    let probNum = prob.ToString()
    let asm = Assembly.GetExecutingAssembly()
    let modul = asm.GetType("ProjectEuler.Problem" + probNum)
    let funcs = modul.GetMember("solve")
    let func = funcs.[0] :?> PropertyInfo
    func.GetValue(modul, null)

let getProblemNumber() = 
    printf "%s" "Enter problem #: "
    let str = Console.ReadLine()
    match Int32.TryParse(str) with
        | true, parsed -> Some parsed
        | _ -> None

let rec eulerRepl() =
    let q = getProblemNumber()
    if q.IsNone then ()
    else
        let tmr = Stopwatch.StartNew()
        
        try 
            let answer = dynamicExecute q.Value 
            printfn "Problem %d answer is %A\nSolved In: %A\n\n" q.Value answer tmr.Elapsed
        with
            | :? System.NullReferenceException -> printfn "Problem %d not solved yet" q.Value
            | _ as ex -> printfn "%s: %s" (ex.GetType().Name) ex.Message
        eulerRepl()

eulerRepl()