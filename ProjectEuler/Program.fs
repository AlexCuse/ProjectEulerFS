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

//let dynamicExecute probNum =
//    let compiler = new FSharpCodeProvider()
//    let code = "#light\nmodule GeneratedCode\nlet GenTest = fun () -> printfn \"%s\" (Problem" + probNum + ".solve)"
//    //let code = "#light\nmodule GeneratedCode\nlet GenTest = fun () -> printfn \"%A\" (\"Problem " + probNum + " answer is: \"" + "(Problem" + probNum + ".doStuff)) \n"
//    let options = new System.CodeDom.Compiler.CompilerParameters()
//    options.GenerateInMemory <- true
//    options.ReferencedAssemblies.Add(System.Reflection.Assembly.GetExecutingAssembly().FullName)|>ignor
//    let cr = compiler.CompileAssemblyFromSource(options, [|code|])
//    printfn "Compiler return value: %d" cr.NativeCompilerReturnValue
//    for s in cr.Output do printfn "%s" s
//    for e in cr.Errors do printfn "%O" e
//    let ass = cr.CompiledAssembly
//    let modul = ass.GetType("GeneratedCode")
//    printfn "module %O" modul
//    let funcs = modul.GetMember("GenTest")
//    let func = funcs.[0] :?> MethodInfo
//    let typ = func.GetType()
//    printfn "member %A of type %A" func typ.FullName
//    func.Invoke(None,[||]) |> ignore
//    printfn "compiler test done."