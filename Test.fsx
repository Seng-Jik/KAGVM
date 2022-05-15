#r "nuget: FParsec"
#load "Parser.fs"

open FParsec.CharParsers
open FParsec.Primitives
open KAGVM.Parser


let testSingleFile failDetails file =
    let content = System.IO.File.ReadAllText file
    printf "%s..." file
    match runParserOnString document () file content with
    | Success _ -> 
        System.Console.ForegroundColor <- System.ConsoleColor.Green
        printfn "OK!"
        System.Console.ResetColor ()
    | err -> 
        if failDetails
        then
            printfn "" 
            printfn "%A" err
        else 
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            printfn "Failed!"
            System.Console.ResetColor ()


let testAll () =
    []
    |> Seq.collect (System.IO.Directory.GetFiles)
    |> Seq.filter (fun x -> x.ToLower().EndsWith ".ks")
    |> Seq.iter (testSingleFile false)


let test = testSingleFile true


testAll()

