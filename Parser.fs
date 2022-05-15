﻿module KAGVM.Parser

open FParsec.CharParsers
open FParsec.Primitives


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


let label: Parser<string * string option, unit> =
    tuple2 (pchar '*' >>. (opt labelName |>> Option.defaultValue ""))
           (pchar '|' >>. (manyCharsTill anyChar lineEnd |>> Some) <|> (lineEnd) |>> fun _ -> None)
    <?> "lable"


type Value = 
    | Symbol of string
    | String of string


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


let value: Parser<Value, _> = 
    choice <| List.map attempt [ Value.string' |>> String 
                                 Value.symbol |>> Symbol ]
    <?> "value"


let argName = identifier <|> pstring "*"


let argument = tuple2 (argName <?> "argName") (opt (pstring "=" >>. value)) 
               <?> "argument"


type Command = 
  { Command: string
    Args: (string * Value option) list }


let ws, ws1 = 
    let s = pchar ' ' <|> pchar '\t'
    many s |>> ignore,
    many1 s |>> ignore


let commandName: Parser<_, unit> = many1Chars <| satisfy (fun x -> x <> ' ' && x <> ']')


let arguments = sepBy1 argument ws1 <?> "arguments"
let command = tuple2 commandName (opt (ws1 >>. arguments)) <?> "command" 
              |>> fun (c: _, a: _) -> { Command = c; Args = Option.defaultValue [] a }


let inlineCommand = between (pchar '[') (pchar ']') command <?> "inline command"
let lineCommand = pchar '@' >>. command <?> "line command"


let textPiece: Parser<_, unit> = 
    let keyChars = 
        set [ '['; ']'; '@'; '\r'; '\n'; '*'; ';' ]
    many1Satisfy (not << fun x -> Set.contains x keyChars)
    <?> "text piece"


type Element = 
    | TextPiece of string
    | Command of Command
    | Tjs of string


type Line = Line of lineNumber: int64 * elements: Element list * comment: string option

            
let tjs: Parser<string, unit> = 
    pstring "[iscript]" >>. manyCharsTill anyChar (pstring "[endscript]") <?> "Tjs"


let bodyLine = 
    notFollowedByEof >>= fun _ ->
    (getPosition |>> fun pos -> pos.Line) .>>.
    begin
        [ tjs |>> Tjs |>> List.singleton
          lineCommand |>> Command |>> List.singleton
          many ((inlineCommand |>> Command) <|> (textPiece |>> TextPiece)) ]
        |> choice
        .>>. lineEnd
    end
    |>> fun (a, (b, c)) -> Line (a, b, c)
    <?> "body line"


type Scene = Scene of labelName: string * labelDisplayName: string option * Line list


let scene =
    tuple2 label (many bodyLine)
    |>> fun ((a, b), c) -> Scene (a, b, c)
    <?> "scene"


let document = (many bodyLine) .>>. many scene <?> "document"