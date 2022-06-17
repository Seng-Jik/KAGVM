#r "nuget: FParsec"
#load "Module.fs"
#load "Parser.fs"

open FParsec.CharParsers
open FParsec.Primitives
open KAGVM.Parser
open KAGVM.Module


let testSingleFile failDetails file =
    printf "%s..." file
    match loadModule file with
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


let testScript parser str = 
    match run parser str with
    | Success (x, _, _) ->
        printfn "%A" x
    | err ->
        printfn "%A" err


let testParse = testScript kagModule


testScript arguments """abc=1 msgoff trans=map38"""

