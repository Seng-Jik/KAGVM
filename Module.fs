module KAGVM.Module


type Command = 
  { Command: string
    Args: (string * string option) list }


type Element = 
    | TextPiece of string
    | Command of Command
    | Tjs of string


type Line = Line of lineNumber: int64 * elements: Element list * comment: string option
type Scene = Scene of labelName: string * labelDisplayName: string option * comment: string option * Line list
type Module = Module of header: Line list * Scene list


