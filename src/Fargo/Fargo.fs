#if !INTERACTIVE
namespace Fargo
#else
#load "Token.fs" "Console.fs" "Parsers.fs"
open Fargo
#endif

open System
open Console

type Completer = string -> string list

[<Flags>]
type UsageType =
| None = 0
| Arg = 1
| Required = 2
| Many = 4

type Usage = { Name: string option; Alt: string option; Value: string option; Description: string; Help: string option; Type: UsageType}
    with
        member this.IsRequired = this.Type &&& UsageType.Required <> enum 0
        member this.IsArg = this.Type &&& UsageType.Arg <> enum 0
        member this.IsMany = this.Type &&& UsageType.Many <> enum 0

type Usages =
    { Path:  Usage list
      Options: Usage list }
type Tokens = Token list

type ParseResult<'t> =
    | Success of 't
    | Failure of string list
    | Complete of string list * important: bool



type Arg<'a> = int voption -> Tokens -> ParseResult<'a> * Tokens * Usages

module Usages =
    let merge x y = { Path = y.Path @ x.Path ; Options = x.Options @ y.Options}

    let empty = {Path = []; Options = []}

module Usage =
    let isMatch usage token =
        (match usage.Name with Some name -> token.Text = name | _ -> false) ||  (match usage.Alt with Some alt -> token.Text = alt | _ -> false)

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
            [ match usage.Name with
              | Some name ->
                if name.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                    name
              | None -> ()
              match usage.Alt with
              | Some alt ->
                    if alt.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                            alt
              | None -> () ], false)

    let req usages =
        match usages.Options with
        | usage :: tail ->
            { usages with
                Options =
                    { usage with
                        Type = usage.Type ||| UsageType.Required }
                    :: tail }
        | _ -> usages


    module Short =
        let ofString (name: string) =
            name
            |> Option.ofObj
            |> Option.map ((+) "-")

[<AutoOpen>]
module Fargo =
    let cmd name alt description: Arg<string> =
        let usage = { Name = Some name; Alt = Option.ofObj alt; Value = None; Description = description; Help = None; Type = UsageType.Required }
        let matchusages = { Path = [ usage ]; Options = [usage]} 
        let failusages = { Path = []; Options = [usage]} 
        let notFound = Failure [$"Command %s{name} not found"]
        fun pos tokens ->
            match tokens with
            | (Usage.IsPrefix pos & cmd) :: rest ->
                Usage.complete usage cmd.Text, tokens, failusages
            | cmd :: tail when Usage.isMatch usage cmd ->
                Success name, tail, matchusages
            | [] ->
                match pos with
                | ValueSome _ -> Usage.complete usage "", tokens, failusages
                | _ -> notFound, tokens, failusages
            | _ -> notFound, tokens, failusages


    let rec private findArg usage usages complete pos tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text , remaining @ tokens, usages
            elif Usage.isMatch usage x  then
                match pos with
                | ValueSome pos ->
                    if pos <= y.Extent.End then
                        Complete (complete y.Text, true), remaining @ tail, usages
                    else
                        Success (Some y.Text), remaining @ tail, usages

                | ValueNone -> Success (Some y.Text), remaining @ tail, usages
            else
                findArg usage usages complete pos rest (remaining @ [x])
        | [x]  ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text , remaining @ tokens, usages
            elif Usage.isMatch usage x then
                match pos with
                | ValueSome pos ->
                    Complete (complete "", true), remaining , usages
                | ValueNone ->
                    let name = usage.Name |> Option.defaultValue "<Unknown>"
                    Failure [$"Argument %s{name} value is missing"], remaining, usages
            else
                match pos with
                | ValueSome _ ->
                    Usage.complete usage "", remaining @ tokens , usages
                | _ ->
                    Success None, remaining @ tokens , usages
        | [] ->
            match pos with
            | ValueSome _ ->
                Usage.complete usage "" , remaining @ tokens, usages
            | ValueNone -> Success None, remaining  , usages

    let opt name alt value description: Arg<string option> =
        let usage = { Name = Some ("--" + name); Alt = Usage.Short.ofString alt; Value = Some value; Description = description; Help = None; Type = UsageType.Arg }
        let usages = { Path = []; Options = [usage]}
        fun pos tokens -> findArg usage usages (fun _ -> []) pos tokens []

    let arg value description: Arg<string option> =
        let usage = { Name = None; Alt = None; Value = Some value; Description = description; Help = None; Type = UsageType.Arg}
        let usages = { Path = []; Options = [usage]}
        fun pos tokens ->
            match tokens with
            | value :: tail ->
                Success (Some value.Text), tail, usages
            | [] ->
                match pos with
                | ValueSome _ -> Complete([],false), tokens, usages
                | _ -> Success None, tokens, usages

    let completer complete (arg: Arg<string option>) : Arg<string option> =
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            match usages.Options with
            | usage :: _ -> 
                findArg usage usages complete pos tokens []
            | _ -> result, tokens, usages

    let reqArg (arg: Arg<'a option>) : Arg<'a> =
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            let reqResult = 
                match result with
                | Success (Some v) -> Success v
                | Success None ->
                    let value =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Value)
                        |> Option.defaultValue "unknown"
                    Failure [$"Required argument <%s{value}> not found"]
                | Failure e -> Failure e
                | Complete (c,i) -> Complete (c,i)
            reqResult, rest, Usage.req usages

    let reqOpt (arg: Arg<'a option>) : Arg<'a> =
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            let reqResult = 
                match result with
                | Success (Some v) -> Success v
                | Success None ->
                    let name =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                    Failure [$"Required argument %s{name} not found"]
                | Failure e -> Failure e
                | Complete (c,i) -> Complete (c,i)
            reqResult, rest, Usage.req usages

    let flag name alt description : Arg<bool> =
        let usage = {Name = Some ("--" + name); Alt = Usage.Short.ofString alt; Value = None; Description = description; Help = None; Type = UsageType.Arg}
        let usages = { Path = []; Options = [usage]} 
        let rec findFlag pos tokens remaining =
            match tokens with
            | x :: rest ->
                if Usage.isStop pos x then
                    Usage.complete usage x.Text, remaining @ tokens, usages 
                elif Usage.isMatch usage x  then
                    Success true, remaining @ rest, usages
                else
                    findFlag pos rest (remaining @ [x])
            | [] ->
                match pos with
                | ValueSome _ ->
                    Usage.complete usage "" , remaining @ tokens, usages
                | ValueNone -> Success false, remaining, usages
        fun pos tokens -> findFlag pos tokens []

    let reqFlag (f: Arg<bool>) : Arg<bool> =
        fun pos tokens ->
            let result, rest, usages = f pos tokens

            let reqResult =
                match result with
                | Success true -> Success true
                | Success false ->
                    let name =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                        
                    Failure [ $"Required flag %s{name} not found"]
                | Failure e -> Failure e
                | Complete (c,i) -> Complete (c,i)
            reqResult, rest, Usage.req usages

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
                    | Success y, resty, usagey -> Success (f x y), resty, Usages.merge usagex usagey
                    | Failure ey, resty, usagey -> Failure ey, resty, Usages.merge usagex usagey
                    | Complete (cy,iy), resty, usagey -> Complete (cy,iy), resty, Usages.merge usagex usagey 
                | Failure ex, restx, usagex -> 
                    match argy pos restx with
                    | Success y, resty, usagey -> Failure ex, resty, Usages.merge usagex usagey
                    | Failure ey, resty, usagey -> Failure (ex@ey), resty, Usages.merge usagex usagey
                    | Complete (cy,iy), resty, usagey -> Complete (cy,iy), resty, Usages.merge usagex usagey
                | Complete (cx,ix), restx, usagex ->
                    match argy pos restx with
                    | Success y, resty, usagey -> Complete (cx,ix), restx, Usages.merge usagex usagey
                    | Failure ey, resty, usagey -> Complete (cx,ix), restx, Usages.merge usagex usagey
                    | Complete (cy,iy), resty, usagey -> 
                        let (c,i) =
                            match ix, iy with
                            | false, false -> cx @ cy, false
                            | true, true -> cx @ cy, true
                            | true, false -> cx, true
                            | false, true -> cy, true
                        Complete (c,i), resty, Usages.merge usagex usagey

    let bind (f: 'a -> Arg<'b>) (x: Arg<'a>) : Arg<'b> =
        fun pos tokens ->
            match x pos tokens with
            | Success x, restx, usagex ->
                    let y, resty, usagey = f x pos restx
                    y, resty,  { Path = usagey.Path @ usagex.Path; Options = usagey.Options}
            | Failure ex, restx, usagex ->
                Failure ex, restx, usagex
            | Complete (c,i), restx, usagex ->
                Complete (c,i), restx, usagex

    let ret (x: 'a) : Arg<'a> =
        fun pos tokens ->
            Success x, tokens, Usages.empty

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
            | (Success x, restx, usagex), (_, _, usagey) -> Success x, restx, {  usagex with Options = usagex.Options @ usagey.Options }
            | (_,_,usagex), (Success y, resty, usagey) -> Success y, resty, { usagey with Options = usagex.Options @ usagey.Options }
            | (Failure ex,_,usagex), (Failure ey, _, usagey) -> Failure ey, tokens, {Path = []; Options = usagex.Options @ usagey.Options }

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
            | (Success (Some x), restx, usagex), (_, _, usagey) -> Success (Some x), restx, {  usagex with Options = usagex.Options @ usagey.Options }
            | (_,_,usagex), (Success (Some y), resty, usagey) -> Success (Some y), resty, {  usagey with Options = usagex.Options @ usagey.Options }
            | (Success None, rest,usagex), (_,_, usagey)
            | (_, _, usagex), ( Success None, rest, usagey ) -> Success None, rest, Usages.merge usagex usagey
            | (Failure _,_,usagex), (Failure ey, _, usagey) -> Failure (ey), tokens, Usages.merge usagex usagey


    let error message : Arg<_> =
        fun pos tokens ->
            Failure [message], tokens, Usages.empty 

    let errorf<'t> messageFunc : Arg<'t> =
        fun pos tokens ->
            let msg = messageFunc tokens
            Failure [msg], tokens, Usages.empty

    let cmdError<'t> : Arg<'t> =
        errorf (function [] -> "Missing command"| token :: _ -> $"Unknown command %s{token.Text}")

    let help text (arg: Arg<'t>) : Arg<'t> =
        fun pos tokens ->
            let result, rest, usages = arg pos tokens
            result, rest, {
                Path = 
                    match usages.Path with
                    | usage :: tail -> { usage with Help = Some text} :: tail
                    | _ -> usages.Path
                Options =
                    match usages.Options with
                    | usage :: tail -> { usage with Help = Some text} :: tail
                    | _ -> usages.Options
            }


    
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
            eprintfn $"%s{Colors.red}%s{err}%s{Colors.def}"

    let printUsage (usages: Usages) =
        printf $"%s{Colors.yellow}Usage: "
        
        for c in List.rev usages.Path do
            c.Name |> Option.defaultValue "unknown" |> printf "%s "
        let cmds =
            usages.Options
            |> List.filter (fun u -> not u.IsArg)

        let args =
            usages.Options
            |> List.filter (fun u -> u.Name = None || u.IsRequired)

        let opts =
            usages.Options
            |> List.filter (fun u -> not (u.Name = None || u.IsRequired))

        if cmds <> [] then printfn "[command]"
        if opts <> [] then printf "[options] "
        for u in args do
            if u.IsArg then
                if not u.IsRequired then
                    printf "["
                String.concat " " [
                    yield! u.Name |> Option.toList
                    yield! u.Value |> Option.map (sprintf "<%s>") |> Option.toList
                ] |> printf "%s"
                if u.IsMany then
                    printf "..."
                 
                if not u.IsRequired then
                    printf "]"
                printf " "
        printfn $"%s{Colors.def}"

    let printDescription usages =
        match usages.Path with
        | usage :: _ ->
            let help = usage.Help |> Option.defaultValue usage.Description
            printfn ""
            printfn "%s" help
            printfn ""

        | [] -> ()

    let printOptions (usages: Usage list) =
        let cmds =
            usages
            |> List.filter (fun u -> not u.IsArg)

        let args =
            usages
            |> List.filter (fun u -> u.IsArg && u.IsRequired)

        let opts =
            usages
            |> List.filter (fun u -> u.IsArg && not u.IsRequired)

        match cmds with
        | [] -> ()
        | _ ->
            printfn $"%s{Colors.yellow}Commands:"
            for c in cmds do
                match c.Name  with
                | Some name ->
                    let cmd =
                        match c.Alt with
                        | None -> name
                        | Some alt -> name + ", " + alt
                    printfn $"    %-24s{cmd}%s{c.Description}"
                | None -> ()

        match args with
        | [] -> ()
        | _ ->
            printfn $"%s{Colors.yellow}Arguments:" 
            for u in args do
                match u.Name with
                | Some name ->
                    let cmd =
                        let n =
                            match u.Alt with
                            | None -> name
                            | Some alt -> name + ", " + alt
                        let v =
                            u.Value
                            |> Option.map (sprintf " <%s> ")
                            |> Option.defaultValue ""
                        n+v
                    printfn $"    %-24s{cmd}%s{u.Description}"
                | None ->
                    match u.Value with
                    | Some v ->
                        let value = $"<%s{v}>"
                        printfn $"    %-24s{ value }%s{u.Description}"
                    | None -> ()
            printfn $"%s{Colors.def}"


        match opts with
        | [] -> ()
        | _ ->
            printfn $"%s{Colors.yellow}Options:" 
            for u in opts do
                match u.Name with
                | Some name ->
                    let cmd =
                        let n =
                            match u.Alt with
                            | None -> name
                            | Some alt -> name + ", " + alt
                        let v =
                            u.Value
                            |> Option.map (sprintf " <%s> ")
                            |> Option.defaultValue ""
                        n+v
                    printfn $"    %-24s{cmd}%s{u.Description}"
                | None ->
                    match u.Value with
                    | Some v ->
                        let value = $"<%s{v}>"
                        printf $"    %-24s{ value }%s{u.Description}"
                    | None -> ()
            printfn $"%s{Colors.def}"

    let printHelp usages =
        printUsage usages
        printDescription usages
        printOptions usages.Options

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
                    | ex -> printErrors [string ex]

                | Error(errs, usage) ->
                    printfn "%s" appName
                    printErrors errs
                    printfn ""
                    printHelp usage

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
                    Success (values.Close()), tokens, Usages.empty
                else
                    Success [], tokens, Usages.empty 
            | ValueSome _ -> Complete([],false), tokens, Usages.empty

    let orStdIn (arg: Arg<string option>) : Arg<string list> =
        arg |> reqOpt |> map (fun x -> [x]) |> alt stdIn
        
module Env =
    let envVar name : Arg<string option> =
        fun pos tokens ->
            match pos with
            | ValueNone ->
                match Environment.GetEnvironmentVariable(name) with
                | null -> Success None, tokens, Usages.empty
                | value -> Success (Some value), tokens, Usages.empty
            | ValueSome _ -> Complete([], false), tokens, Usages.empty
                



        
