module KAGVM.Parser

open FParsec.CharParsers
open FParsec.Primitives
open Module


let comment: Parser<string, unit> = 
    pchar ';' >>. manyCharsTill anyChar (newlineReturn () <|> eof)
    <?> "comment"


let lineEnd = 
    (comment |>> Some) <|> ((newlineReturn () <|> eof) |>> (fun _ -> None))
    <?> "lineEnd"


let identifier: Parser<_, unit> = identifier <| IdentifierOptions ()


let labelName: Parser<_, unit> = 
    many1Chars <| 
        satisfy (fun x -> x <> '\n' && x <> '\r' && x <> '|')


let label: Parser<string * string option * string option, unit> =
    let displayName = 
        manyCharsTillApply anyChar lineEnd (fun displayName comment -> 
            Some displayName, comment)
    tuple2 (pchar '*' >>. (opt labelName |>> Option.defaultValue ""))
           (pchar '|' >>. displayName <|> (lineEnd |>> fun comment -> None, comment))
    |>> fun (a, (b, c)) -> a, b, c
    <?> "lable"


let stringLiteral =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pchar '\"') (pchar '\"')
            (stringsSepBy normalCharSnippet escapedChar)
    <?> "stringLiteral"


module Value =
    let symbol = manySatisfy (fun x -> 
        x <> ' ' && x <> '\"' && x <> ']' && x <> '[' && x <> '\n' && x <> '\r')
    let string': Parser<_, unit> = stringLiteral <?> "string"


let value: Parser<string, _> = 
    choice <| List.map attempt [ Value.string' 
                                 Value.symbol ]
    <?> "value"


let argName = identifier <|> pstring "*"


let ws, ws1 = 
    let s = pchar ' ' <|> pchar '\t'
    many s |>> ignore,
    many1 s |>> ignore


let argument = tuple2 (argName <?> "argName") (ws >>. opt (pstring "=" >>. ws >>. value)) 
               <?> "argument"


let commandName: Parser<_, unit> = many1Chars <| satisfy (fun x -> x <> ' ' && x <> ']' && x <> ';' && x <> '\n' && x <> '\r')


let arguments = sepBy1 argument ws1 <?> "arguments"
let command = tuple2 commandName (opt <| attempt (ws1 >>. arguments)) <?> "command" 
              |>> fun (c: _, a: _) -> { Command = c; Args = Option.defaultValue [] a }


let inlineCommand = between (pchar '[') (pchar ']') command <?> "inline command"
let lineCommand = pchar '@' >>. command <?> "line command"


let textPiece: Parser<_, unit> = 
    let keyChars = 
        set [ '['; ']'; '@'; '\r'; '\n'; '*'; ';' ]
    many1Satisfy (not << fun x -> Set.contains x keyChars)
    <?> "text piece"


let tjs: Parser<string, unit> = 
    (pstring "[iscript]" >>. manyCharsTill anyChar (pstring "[endscript]"))
    <|> ((pstring "@iscript" >>. manyCharsTill anyChar (pstring "@endscript")))
    <?> "Tjs"
    


let bodyLine = 
    notFollowedByEof >>= fun _ ->
    (getPosition |>> fun pos -> pos.Line) .>>.
    begin
        [ tjs |>> Tjs |>> List.singleton
          lineCommand |>> Command |>> List.singleton .>> ws
          many ((inlineCommand |>> Command) <|> (textPiece |>> TextPiece)) ]
        |> choice
        .>>. lineEnd
    end
    |>> fun (a, (b, c)) -> Line (a, b, c)
    <?> "body line"


let scene =
    tuple2 label (many bodyLine)
    |>> fun ((a, b, c), d) -> Scene (a, b, c, d)
    <?> "scene"


let kagModule = 
    (many bodyLine) .>>. many scene <?> "module"
    |>> Module


let parseModule name content =
    runParserOnString kagModule () name content


let loadModule path = 
    let content = System.IO.File.ReadAllText path
    parseModule path content


let loadModule' =
    loadModule >> function
        | Success (x, _, _) -> x
        | Failure _ as x -> failwithf "%A" x

        