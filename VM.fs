module KAGVM.VM

open KAGVM.Module


type VM<'state> = 
  { RunModule: Module -> 'state -> 'state 
    RunScene: Module -> Scene -> 'state -> 'state
    RunLine: Module -> Scene -> Line -> 'state -> 'state
    RunElement: Module -> Scene -> Line -> Element -> 'state -> 'state
    RunTextPiece: Module -> Scene -> Line -> string -> 'state -> 'state
    RunCommand: Module -> Scene -> Line -> Command -> 'state -> 'state 
    RunTjs: Module -> Scene -> Line -> string -> 'state -> 'state }


let empty =
  { RunModule = fun _ -> id
    RunScene = fun _ _ -> id
    RunLine = fun _ _ _ -> id
    RunElement = fun _ _ _ _ -> id
    RunTextPiece = fun _ _ _ _ -> id 
    RunCommand = fun _ _ _ _ -> id 
    RunTjs = fun _ _ _ _ -> id }


module CommandDispatcher =

    type CommandDispatcherState =
      { CurModule: Module
        CurScene: Scene 
        CurLine: Line }


    type CommandHandler<'state> = CommandDispatcherState -> Command -> 'state -> 'state


    let dispatchCommands (commands: Map<string, CommandHandler<'state'>>) =
        fun modu scene line (cmd: Command) state ->
            match Map.tryFind cmd.Command commands with
            | None -> 
                printfn "Warning: Can not process command %s." cmd.Command
                state
            | Some handler -> 
                let curState = 
                    { CurModule = modu; CurScene = scene; CurLine = line }
                
                handler curState cmd state


let runTextPiece vm m scene line piece state =
    vm.RunTextPiece m scene line piece state


let runCommand vm m scene line cmd state =
    vm.RunCommand m scene line cmd state


let runTjs vm m scene line tjs state =
    vm.RunTjs m scene line tjs state


let runElement vm m scene line element state =
    let state = vm.RunElement m scene line element state
    match element with
    | TextPiece t -> runTextPiece vm m scene line t state
    | Tjs t -> runTjs vm m scene line t state
    | Command c -> runCommand vm m scene line c state


let runLine vm m scene line state =
    let state = vm.RunLine m scene line state
    let (Line (_, elements, _)) = line
    List.fold (fun s e -> runElement vm m scene line e s) state elements


let runScene vm m scene state =
    let state = vm.RunScene m scene state
    let (Scene (_, _, _, e)) = scene
    List.fold (fun s e -> runLine vm m scene e s) state e


let runModule vm m state =
    let state = vm.RunModule m state
    let (Module (header, scenes)) = m
    match header with
    | [] -> scenes
    | _ -> Scene ("**init**", None, None, header) :: scenes
    |> List.fold (fun s e -> runScene vm m e s) state

