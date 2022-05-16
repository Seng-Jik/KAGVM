module KAGVM.Module


type Command = 
  { Command: string
    Args: (string * string option) list }


let arg argName { Command = _; Args = a } = 
    List.tryFind (fst >> (=) argName) a 
    |> Option.map snd
    |> Option.flatten


let arg' argName cmd = (arg argName cmd).Value


let commandToYukimiScript wrappedArgs c =
    "@" + c.Command + 
    begin
        List.fold 
            (fun s (x, n) -> 
                let argName = " --" + x
                let wrap = 
                    if List.exists ((=) x) wrappedArgs
                    then "\""
                    else ""
                let argValue = Option.map (fun x -> " " + wrap + x + wrap) n
                s + argName + Option.defaultValue "" argValue)
            "" 
            c.Args
    end


type Element = 
    | TextPiece of string
    | Command of Command
    | Tjs of string


type Line = Line of lineNumber: int64 * elements: Element list * comment: string option
type Scene = Scene of labelName: string * labelDisplayName: string option * comment: string option * Line list
type Module = Module of header: Line list * Scene list


type CommandDeclaretion = 
    { Command: string 
      Params: string list }


let commandDeclaretions m =
    let genParams (command: Command) =
        command.Args |> List.map fst |> set

    let addDecl (declaretions: Map<string, Set<string>>) (command) =
        let decl = genParams command
        match Map.tryFind command.Command declaretions with
        | None -> Map.add command.Command decl declaretions
        | Some x -> 
            Map.add command.Command (Set.union x decl) declaretions

    let addDecl' decls = 
        function
        | Command cmd -> addDecl decls cmd
        | _ -> decls

    let addDecl'' decls (Line (_, e, _)) =
        List.fold addDecl' decls e
        
    let addDecl''' decls (Scene (_, _, _, e)) =
        List.fold addDecl'' decls e

    let addDecl'''' decl (Module (header, scenes)) =        
        List.fold addDecl''' (List.fold addDecl'' decl header) scenes

    let decls = List.fold addDecl'''' Map.empty

    decls m
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> 
        { Command = k; Params = Set.toList v })
    |> Seq.toList


let printDeclaretions =
    List.iter (fun x -> 
        printfn "%s %A" x.Command x.Params)


let sceneNames (Module (_, scenes)) = 
    scenes 
    |> List.map (fun (Scene (labelName, labelDisplayName, _, _)) -> 
        labelName, labelDisplayName)
            