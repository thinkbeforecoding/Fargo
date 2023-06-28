namespace Fargo

open System
open Console

type Completer = string -> string list

type Usage = { Name: string; Alt: string option; Description: string}
type Usages = Usage list
type Tokens = Token list

type ParseResult<'t> =
    | Success of 't
    | Failure of string list
    | Complete of string list * important: bool



type Arg<'a> = int voption -> Tokens -> ParseResult<'a> * Tokens * Usages


module Usage =
    let isMatch usage token =
        token.Text = usage.Name ||  (match usage.Alt with Some alt -> token.Text = alt | _ -> false)

    let isStop pos token =
        match pos with
        | ValueSome pos -> Extent.contains pos token.Extent
        | ValueNone -> false
    
    let (|IsPrefix|_|) pos token =
        if isStop pos token then
            Some()
        else
            None

    let complete usage input =
        Complete (
            [ if usage.Name.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                usage.Name
              match usage.Alt with
               | Some alt ->
                    if alt.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                            alt
               | None -> () ], false)

    let change f usages =
        match usages with
        | usage :: tail ->
            { Name = usage.Name
              Alt = usage.Alt
              Description = f usage.Description }
            :: tail
        | _ -> usages

    module Short =
        let ofString (name: string) =
            name
            |> Option.ofObj
            |> Option.map ((+) "-")

[<AutoOpen>]
module Fargo =
    let cmd name alt description: Arg<string> =
        let usage = { Name = name; Alt = Option.ofObj alt; Description = description}
        fun pos tokens ->
            match tokens with
            | (Usage.IsPrefix pos & cmd) :: rest ->
                Usage.complete usage cmd.Text, tokens, [usage]
            | cmd :: tail when Usage.isMatch usage cmd ->
                Success name, tail, [usage]
            | [] ->
                match pos with
                | ValueSome _ -> 
                    Usage.complete usage "", tokens, [usage]
                | _ ->
                    Failure [$"Command %s{usage.Name} not found"], tokens, [usage]
                
            | _ ->
                Failure [$"Command %s{usage.Name} not found"], tokens, [usage]


    let rec private findArg usage complete pos tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text , remaining @ tokens, [usage]
            elif Usage.isMatch usage x  then
                match pos with
                | ValueSome pos ->
                    if pos <= y.Extent.End then
                        Complete (complete y.Text, true), remaining @ tail, [usage]
                    else
                        Success (Some y.Text), remaining @ tail, [usage]

                | ValueNone -> Success (Some y.Text), remaining @ tail, [usage]
            else
                findArg usage complete pos rest (remaining @ [x])
        | [x]  ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text , remaining @ tokens, [usage]
            elif Usage.isMatch usage x then
                match pos with
                | ValueSome pos ->
                    Complete (complete "", true), remaining , [ usage]
                | ValueNone -> Failure [$"Argument %s{usage.Name} value is missing"], remaining, [ usage ] 
            else
                match pos with
                | ValueSome pos ->
                    Usage.complete usage "", remaining @ tokens , [ usage]
                | _ ->
                    Success None, remaining @ tokens , [ usage ]
        | [] ->
            match pos with
            | ValueSome _ ->
                Usage.complete usage "" , remaining @ tokens, [usage]
            | ValueNone -> Success None, remaining  , [ usage ]

    let arg name alt description: Arg<string option> =
        let usage = { Name = "--" + name; Alt = Usage.Short.ofString alt; Description = description}
        fun pos tokens -> findArg usage (fun _ -> []) pos tokens []

    let completer complete (arg: Arg<string option>) : Arg<string option> =
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            match usages with
            | usage :: _ -> 
                findArg usage complete pos tokens []
            | _ -> result, tokens, usages


    let reqArg (arg: Arg<'a option>) : Arg<'a> =
        let reqUsage description = description + " (required)"
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            let reqResult = 
                match result with
                | Success (Some v) -> Success v
                | Success None ->
                    let name =
                        usages
                        |> List.tryHead
                        |> Option.map (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                    Failure [$"Required argument %s{name} not found"]
                | Failure e -> Failure e
                | Complete (c,i) -> Complete (c,i)
            reqResult, rest, Usage.change reqUsage usages

    let flag name alt description : Arg<bool> =
        let usage = {Name = "--" + name; Alt = Usage.Short.ofString alt; Description = description}
        let rec findFlag pos tokens remaining =
            match tokens with
            | x :: rest ->
                if Usage.isStop pos x then
                    Usage.complete usage x.Text, remaining @ tokens, [usage] 
                elif Usage.isMatch usage x  then
                    Success true, remaining @ rest, [usage]
                else
                    findFlag pos rest (remaining @ [x])
            | [] ->
                match pos with
                | ValueSome _ ->
                    Usage.complete usage "" , remaining @ tokens, [usage]
                | ValueNone -> Success false, remaining, [usage]
        fun pos tokens -> findFlag pos tokens []

    let reqFlag (f: Arg<bool>) : Arg<bool> =
        let reqUsage desc = desc + " (required)"

        fun pos tokens ->
            let result, rest, usages = f pos tokens

            let reqResult =
                match result with
                | Success true -> Success true
                | Success false ->
                    let name =
                        usages
                        |> List.tryHead
                        |> Option.map (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                        
                    Failure [ $"Required flag %s{name} not found"]
                | Failure e -> Failure e
                | Complete (c,i) -> Complete (c,i)
            reqResult, rest, Usage.change reqUsage usages

    let parse (f: 'a -> Result<'b, string>) (arg: Arg<'a>) : Arg<'b>  =
        fun pos tokens ->
            match arg pos tokens with
            | Success x, rest, usage ->
                match f x with
                | Ok v -> Success v, rest, usage
                | Error e -> Failure [e], tokens, usage
            | Failure ex, rest, usage ->
                Failure ex, rest, usage
            | Complete (c,i), rest, usage -> Complete (c,i), rest, usage

    let optParse (f: 'a -> Result<'b, string>) (arg: Arg<'a option>) : Arg<'b option>  =
        fun pos tokens ->
            match arg pos tokens with
            | Success (Some x), rest, usage ->
                match f x with
                | Ok v -> Success (Some v), rest, usage
                | Error e -> Failure [e], tokens, usage
            | Success None, rest, usage -> 
                Success None, rest, usage
            | Failure ex, rest, usage ->
                Failure ex, rest, usage
            | Complete (c,i), rest, usage ->
                Complete (c,i), rest, usage


    let listParse (f: 'a -> Result<'b, string>) (arg: Arg<'a list>) : Arg<'b list> =
        fun pos tokens ->
            match arg pos tokens with
            | Success xs, rest, usage ->
                let results = xs |> List.map f
                let errors = results |> List.collect (function Error e -> [e] | _ -> [])
                match errors with
                | [] -> 
                    let values = results |> List.collect (function Ok v -> [v] | _ -> []) 
                    Success values, rest, usage
                | _ -> 
                    Failure errors, tokens, usage
            | Failure ex, rest, usage ->
                Failure ex, rest, usage
            | Complete (c,i), rest, usage ->
                Complete (c,i), rest, usage

    let nonEmpty error (arg: Arg<'a list>) : Arg<'a list> =
        fun pos tokens ->
            match arg pos tokens with
            | Success [], rest, usage -> Failure [error], rest, usage
            | Success v, rest, usage -> Success v, rest, usage
            | Failure e, rest, usage -> Failure e, rest, usage 
            | Complete (c,i), rest, usage -> Complete(c,i), rest, usage

    let map (f: 'a -> 'b) (arg: Arg<'a>) : Arg<'b> =
        fun pos tokens ->
            match arg pos tokens with
            | Success x, rest, usage-> Success (f x), rest, usage
            | Failure e, rest, usage -> Failure e, rest, usage 
            | Complete (c,i), rest, usage -> Complete (c,i) , rest, usage 

    let optMap f arg = map (Option.map f) arg

    let defaultValue (d: 'a) (arg: Arg<'a option>) : Arg<'a> =
            fun pos tokens ->
                match arg pos tokens with
                | Success None, rest, usage -> Success d, rest, usage
                | Success (Some v), rest, usage -> Success v, rest, usage
                | Failure e, rest, usage -> Failure e, tokens, usage
                | Complete (c,i), rest, usage -> Complete (c,i), rest, usage

    let map2 (f: 'a -> 'b -> 'c) (argx: Arg<'a>) (argy:Arg<'b>) : Arg<'c> =
            fun pos tokens ->
                match argx pos tokens with
                | Success x, restx, usagex -> 
                    match argy pos restx with
                    | Success y, resty, usagey -> Success (f x y), resty, usagex @ usagey
                    | Failure ey, resty, usagey -> Failure ey, resty, usagex @ usagey
                    | Complete (cy,iy), resty, usagey -> Complete (cy,iy), resty, usagex @ usagey
                | Failure ex, restx, usagex -> 
                    match argy pos restx with
                    | Success y, resty, usagey -> Failure ex, resty, usagex @ usagey
                    | Failure ey, resty, usagey -> Failure (ex@ey), resty, usagex @ usagey
                    | Complete (cy,iy), resty, usagey -> Complete (cy,iy), resty, usagex @ usagey
                | Complete (cx,ix), restx, usagex ->
                    match argy pos restx with
                    | Success y, resty, usagey -> Complete (cx,ix), restx, usagex @ usagey
                    | Failure ey, resty, usagey -> Complete (cx,ix), restx, usagex @ usagey
                    | Complete (cy,iy), resty, usagey -> 
                        let (c,i) =
                            match ix, iy with
                            | false, false -> cx @ cy, false
                            | true, true -> cx @ cy, true
                            | true, false -> cx, true
                            | false, true -> cy, true
                        Complete (c,i), resty, usagex @ usagey

    let bind (f: 'a -> Arg<'b>) (x: Arg<'a>) : Arg<'b> =
        fun pos tokens ->
            match x pos tokens with
            | Success x, restx, usagex ->
                    let y, resty, usagey = f x pos restx
                    y, resty,  usagey
            | Failure ex, restx, usagex ->
                Failure ex, restx, usagex
            | Complete (c,i), restx, usagex ->
                Complete (c,i), restx, usagex

    let ret (x: 'a) : Arg<'a> =
        fun pos tokens ->
            Success x, tokens, []

    type FargoBuilder() =
        member _.Bind(x,f) = bind f x

        member _.BindReturn(x : Arg<'a>,f: 'a -> 'b) : Arg<'b> = 
            map f x

        member _.MergeSources(x : Arg<'a>,y: Arg<'b>) : Arg<'a * 'b> = 
            map2 (fun x y -> x,y) x y

        member _.Return(x) = ret x
        member _.ReturnFrom(x: Arg<'a>) = x

        member _.Zero() = ret ()

    let fargo = FargoBuilder()

    let alt (argy: Arg<_>) (argx: Arg<_>) : Arg<_> =
        fun pos tokens ->
            match argx pos tokens, argy pos tokens with
            | (Complete (cx,ix) , restx, usagex), (Complete (cy,iy), _,_) -> 
                let c,i =
                    match ix,iy with
                    | false, false -> cx @ cy, false
                    | true, true -> cx @ cy, true
                    | true, false -> cx, true
                    | false, true -> cy, true
                Complete (c,i) , restx, usagex
            | (Complete (cx,ix) , restx, usagex), _ -> Complete (cx,ix) , restx, usagex
            | _, (Complete (cy,iy) , resty, usagey) -> Complete (cy,iy), resty, usagey
            | (Success x, restx, usagex), (_, _, usagey) -> Success x, restx, usagex@usagey
            | (_,_,usagex), (Success y, resty, usagey) -> Success y, resty, usagex@usagey
            | (Failure ex,_,usagex), (Failure ey, _, usagey) -> Failure (ey), tokens, usagex@usagey

    let optAlt (argy: Arg<'a option>) (argx: Arg<'a option>) : Arg<'a option> =
            fun pos tokens ->
            match argx pos tokens, argy pos tokens with
            | (Complete (cx,ix) , restx, usagex), (Complete (cy,iy), _,_) -> 
                let c,i =
                    match ix,iy with
                    | false, false -> cx @ cy, false
                    | true, true -> cx @ cy, true
                    | true, false -> cx, true
                    | false, true -> cy, true
                Complete (c,i) , restx, usagex
            | (Complete (cx,ix) , restx, usagex), _ -> Complete (cx,ix) , restx, usagex
            | _, (Complete (cy,iy) , resty, usagey) -> Complete (cy,iy), resty, usagey
            | (Success (Some x), restx, usagex), (_, _, usagey) -> Success (Some x), restx, usagex@usagey
            | (_,_,usagex), (Success (Some y), resty, usagey) -> Success (Some y), resty, usagex@usagey
            | (Success None, rest,usagex), (_,_, usagey)
            | (_, _, usagex), ( Success None, rest, usagey ) -> Success None, rest, usagex@usagey
            | (Failure _,_,usagex), (Failure ey, _, usagey) -> Failure (ey), tokens, usagex@usagey


    let error message : Arg<_> =
        fun pos tokens ->
            Failure [message], tokens, [] 

    let errorf<'t> messageFunc : Arg<'t> =
        fun pos tokens ->
            let msg = messageFunc tokens
            Failure [msg], tokens, [] 

    let cmdError<'t> : Arg<'t> =
        errorf (function [] -> "Missing command"| token :: _ -> $"Unknown command %s{token.Text}")


    
    let tryParseTokens (arg: Arg<'a>) tokens =
        let (|Help|_|) tokens = 
            if List.exists (fun t -> t.Text = "--help") tokens then
                Some()
            else
                None

        match arg ValueNone tokens with
        | _, Help, usage -> Error([], usage)
        | Success x, [], _ -> Ok x
        | Success _, cmd :: _, usage -> Error ([$"Unknown command line argument '%s{cmd.Text}'"], usage)
        | Failure e, _, usage -> Error (e, usage)
        | Complete _, _, usage -> Error ([ "Unexpected completion" ], usage)

    let complete (arg: Arg<_>) (pos: int) tokens =

        match arg (ValueSome pos) tokens with
        | Complete (choices,_), _,_ ->
            choices
        |  _,_,_ -> []




    let printErrors errs =
        for err in errs do
            eprintfn $"{Colors.red}{err}{Colors.def}"

    let printUsage (usage: Usage list) =
        printfn $"{Colors.yellow}Usage:" 
        for u in usage do
            let cmd =
                match u.Alt with
                | None -> u.Name
                | Some alt -> u.Name + ", " + alt
            printfn $"\t%-24s{cmd}%s{u.Description}"
        printfn $"{Colors.def}"

    let private (|Int|_|) (input: string) =
        Parsers.Int32.tryParse input

    let run appName arg (cmdLine: string[]) f =
        match Array.toList cmdLine with
        |  "complete" :: rest ->
            let pos, tail =
                match rest with
                | "--position" :: Int pos :: tail -> pos, tail 
                | _ -> Int32.MaxValue, rest
            let tokens =
                match Token.ofCmdLine tail with
                | { Text = t} :: rest when t = appName -> rest
                | list -> list
            for result in complete arg pos tokens do
                printfn "%s" result 
            0
        | "completion" :: "powershell" :: _ ->
            printfn """
    Register-ArgumentCompleter -Native  -CommandName %s -ScriptBlock {
        param($commandName, $wordToComplete, $cursorPosition)
            %s complete --position $cursorPosition "$wordToComplete" | ForEach-Object {
            [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
            }
    }        
            """ appName appName
            0
        | args ->
            try 
                match tryParseTokens arg (Token.ofCmdLine args) with
                | Ok cmd ->
                    try
                        f cmd
                    with
                    | ex -> printErrors [ex]

                | Error(errs, usage) ->
                    printfn "%s" appName
                    printErrors errs
                    printfn ""
                    printUsage usage
                0
            with
            | ex ->
                eprintfn "%O" ex
                -1


module Completer =
    let empty (s: string) : string list = []

    let choices (choices: string list) (s: string) =
        [ for c in choices do
            if c.StartsWith(s) then
                c
        ]

module Opertators =
    let (<|>) x y = x |> alt y
    let (<|?>) x y = x |> optAlt y
    let (|>>) x v = x |> map (fun _ -> v) 

module Pipe =
    open FSharp.Core.CompilerServices
    
    let stdIn : Arg<string list> =
        fun pos tokens -> 
            match pos with
            | ValueNone ->
                if Console.IsInputRedirected then
                    let mutable values = ListCollector()
                    let mutable line = Console.In.ReadLine()
                    while line <> null do
                        values.Add(line)
                        line <- Console.In.ReadLine()
                    Success (values.Close()), tokens, []
                else
                    Success [], tokens, [] 
            | ValueSome _ -> Complete([],false), tokens, []

    let orStdIn (arg: Arg<string option>) : Arg<string list> =
        arg |> reqArg |> map (fun x -> [x]) |> alt stdIn
        
module Env =
    let envVar name : Arg<string option> =
        fun pos tokens ->
            match pos with
            | ValueNone ->
                match Environment.GetEnvironmentVariable(name) with
                | null -> Success None, tokens, []
                | value -> Success (Some value), tokens, []
            | ValueSome _ -> Complete([], false), tokens, []
                



        
