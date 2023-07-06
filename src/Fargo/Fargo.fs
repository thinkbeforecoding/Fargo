#if !INTERACTIVE
namespace Fargo
#else
#load "Token.fs" "Console.fs" "Parsers.fs"
open Fargo
#endif

open System
open Console

[<AutoOpen>]
module Core =
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

    type ParseResult<'t> = Result<'t, string list>



    type Arg<'a> = (Tokens -> ParseResult<'a> * Tokens * Usages) * (int -> Tokens -> string list * bool)

module Usages =
    let merge x y = { Path = y.Path @ x.Path ; Options = x.Options @ y.Options}

    let empty = {Path = []; Options = []}

module Usage =
    let isMatch usage token =
        (match usage.Name with Some name -> token.Text = name | _ -> false) ||  (match usage.Alt with Some alt -> token.Text = alt | _ -> false)

    let isStop pos token =
        Extent.contains pos token.Extent
    
    let (|IsPrefix|_|) pos token =
        if isStop pos token then
            Some()
        else
            None

    let complete usage input =
            [ match usage.Name with
              | Some name ->
                if name.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                    name
              | None -> ()
              match usage.Alt with
              | Some alt ->
                    if alt.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                            alt
              | None -> () ], false

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
        let notFound = Error [$"Command %s{name} not found"]
        let parse tokens =
            match tokens with
            | cmd :: tail when Usage.isMatch usage cmd ->
                Ok name, tail, matchusages
            | _ -> notFound, tokens, failusages
        let complete pos tokens = 
            match tokens with
            | (Usage.IsPrefix pos & cmd) :: rest ->
                Usage.complete usage cmd.Text
            | [] ->
                Usage.complete usage ""
            | _ -> [], false
        parse, complete


    let rec private findArg usage usages tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isMatch usage x  then
                Ok (Some y.Text), remaining @ tail, usages
            else
                findArg usage usages rest (remaining @ [x])
        | [x]  ->
            if Usage.isMatch usage x then
                    let name = usage.Name |> Option.defaultValue "<Unknown>"
                    Error [$"Argument %s{name} value is missing"], remaining, usages
            else
                    Ok None, remaining @ tokens , usages
        | [] ->
            Ok None, remaining  , usages

    let rec private completeArg usage usages complete pos tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text 
            elif Usage.isMatch usage x  then
                    if pos <= y.Extent.End then
                        (complete y.Text tokens, true)
                    else
                        [], false
            else
                completeArg usage usages complete pos rest (remaining @ [x])

        | [x]  ->
            if Usage.isStop pos x then
                Usage.complete usage x.Text 
            elif Usage.isMatch usage x then
                    complete "" tokens, true
            else
                    Usage.complete usage ""
        | [] ->
                Usage.complete usage ""

    let optc name alt value description completer: Arg<string option> =
        let usage = { Name = Some ("--" + name); Alt = Usage.Short.ofString alt; Value = Some value; Description = description; Help = None; Type = UsageType.Arg }
        let usages = { Path = []; Options = [usage]}
        let parse tokens = findArg usage usages tokens []
        let complete pos tokens = completeArg usage usages completer pos tokens []

        parse, complete

    let opt name alt value description: Arg<string option> =
        optc name alt value description (fun _ _ -> [])


    let argc value description completer: Arg<string option> =
        let usage = { Name = None; Alt = None; Value = Some value; Description = description; Help = None; Type = UsageType.Arg}
        let usages = { Path = []; Options = [usage]}
        let parse  tokens =
            match tokens with
            | value :: tail ->
                Ok (Some value.Text), tail, usages
            | [] ->
                Ok None, tokens, usages
        let complete pos tokens =
            match tokens with
            | value :: tail ->
                completer value.Text tail, false
            | [] -> completer "" [], false
        parse, complete

    let arg value description =
        argc value description (fun _ _ -> [])

                
    let reqArg ((p,c): Arg<'a option>) : Arg<'a> =
        let parse tokens =
            let result, rest, usages = p tokens
            let reqResult = 
                match result with
                | Ok (Some v) -> Ok v
                | Ok None ->
                    let value =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Value)
                        |> Option.defaultValue "unknown"
                    Error [$"Required argument <%s{value}> not found"]
                | Error e -> Error e
            reqResult, rest, Usage.req usages
        parse, c

    let reqOpt ((p,c): Arg<'a option>) : Arg<'a> =
        let parse tokens =
            let result, rest, usages = p tokens
            let reqResult = 
                match result with
                | Ok (Some v) -> Ok v
                | Ok None ->
                    let name =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                    Error [$"Required argument %s{name} not found"]
                | Error e -> Error e
            reqResult, rest, Usage.req usages
        parse, c

    let flag name alt description : Arg<bool> =
        let usage = {Name = Some ("--" + name); Alt = Usage.Short.ofString alt; Value = None; Description = description; Help = None; Type = UsageType.Arg}
        let usages = { Path = []; Options = [usage]} 
        let rec findFlag tokens remaining =
            match tokens with
            | x :: rest ->
                if Usage.isMatch usage x  then
                    Ok true, remaining @ rest, usages
                else
                    findFlag rest (remaining @ [x])
            | [] ->
                Ok false, remaining, usages
        let rec completeFlag pos tokens =
            match tokens with
            | x :: rest ->
                if Usage.isStop pos x then
                    Usage.complete usage x.Text
                elif Usage.isMatch usage x  then
                    [], false
                else
                    completeFlag pos rest 
            | [] ->
                Usage.complete usage ""
        let parse tokens = findFlag tokens []
        let complete pos tokens = completeFlag pos tokens 
        parse, complete



    let reqFlag ((p,c): Arg<bool>) : Arg<bool> =
        let parse tokens =
            let result, rest, usages = p tokens

            let reqResult =
                match result with
                | Ok true -> Ok true
                | Ok false ->
                    let name =
                        usages.Options
                        |> List.tryHead
                        |> Option.bind (fun u -> u.Name)
                        |> Option.defaultValue "unknown"
                        
                    Error [ $"Required flag %s{name} not found"]
                | Error e -> Error e
            reqResult, rest, Usage.req usages
        parse, c

    let parse (f: 'a -> Result<'b, string>) ((p,c): Arg<'a>) : Arg<'b>  =
        let parse tokens =
            match p  tokens with
            | Ok x, rest, usage ->
                match f x with
                | Ok v -> Ok v, rest, usage
                | Error e -> Error [e], tokens, usage
            | Error ex, rest, usage ->
                Error ex, rest, usage
        parse, c

    let optParse (f: 'a -> Result<'b, string>) ((p,c): Arg<'a option>) : Arg<'b option>  =
        let parse tokens =
            match p tokens with
            | Ok (Some x), rest, usage ->
                match f x with
                | Ok v -> Ok (Some v), rest, usage
                | Error e -> Error [e], tokens, usage
            | Ok None, rest, usage -> 
                Ok None, rest, usage
            | Error ex, rest, usage ->
                Error ex, rest, usage
        parse, c


    let listParse (f: 'a -> Result<'b, string>) ((p,c): Arg<'a list>) : Arg<'b list> =
        let parse tokens =
            match p  tokens with
            | Ok xs, rest, usage ->
                let results = xs |> List.map f
                let errors = results |> List.collect (function Error e -> [e] | _ -> [])
                match errors with
                | [] -> 
                    let values = results |> List.collect (function Ok v -> [v] | _ -> []) 
                    Ok values, rest, usage
                | _ -> 
                    Error errors, tokens, usage
            | Error ex, rest, usage ->
                Error ex, rest, usage
        parse, c

    let all value description : Arg<Token list> =
        let parse tokens =
            Ok tokens, [], { Path = []; Options = [{ Name = None; Alt = None; Value = Some value; Description = description; Help = None; Type = UsageType.Arg }] }
        let complete _ _ = [], false
        parse, complete

    let validate (f: 'a -> bool) error ((p,c): Arg<'a>) : Arg<'a> =
        let parse tokens =
            let result, tokens, usages = p tokens
            match result with
            | Ok v when not (f v) ->
                Error [error], tokens, usages
            | _ -> result, tokens, usages
        parse, c
    
    let optValidate (f: 'a -> bool) error (arg: Arg<'a option>) : Arg<'a option> =
        validate (function Some v -> f v | None -> true) error arg


    let nonEmpty error ((p,c): Arg<'a list>) : Arg<'a list> =
        let parse tokens=
            match p tokens with
            | Ok [], rest, usage -> Error [error], rest, usage
            | Ok v, rest, usage -> Ok v, rest, usage
            | Error e, rest, usage -> Error e, rest, usage 
        parse, c

    let map (f: 'a -> 'b) ((p,c): Arg<'a>) : Arg<'b> =
        let parse tokens =
            match p tokens with
            | Ok x, rest, usage-> Ok (f x), rest, usage
            | Error e, rest, usage -> Error e, rest, usage 
        parse,c

    let optMap f arg = map (Option.map f) arg

    let defaultValue (d: 'a) ((p,c): Arg<'a option>) : Arg<'a> =
        let parse tokens =
            match p tokens with
            | Ok None, rest, usage -> Ok d, rest, usage
            | Ok (Some v), rest, usage -> Ok v, rest, usage
            | Error e, rest, usage -> Error e, tokens, usage
        parse,c

    let map2 (f: 'a -> 'b -> 'c) ((px,cx): Arg<'a>) ((py,cy):Arg<'b>) : Arg<'c> =
            let parse tokens =
                match px tokens with
                | Ok x, restx, usagex -> 
                    match py restx with
                    | Ok y, resty, usagey -> Ok (f x y), resty, Usages.merge usagex usagey
                    | Error ey, resty, usagey -> Error ey, resty, Usages.merge usagex usagey
                | Error ex, restx, usagex -> 
                    match py restx with
                    | Ok y, resty, usagey -> Error ex, resty, Usages.merge usagex usagey
                    | Error ey, resty, usagey -> Error (ex@ey), resty, Usages.merge usagex usagey
            let complete pos tokens =
                let cpx, ix = cx pos tokens 
                let cpy, iy = cy pos tokens
                match ix, iy with
                | true, false -> cpx, true
                | false, true -> cpy, true
                | true, true -> cpx @ cpy , true
                | false, false -> cpx @ cpy , false
            parse, complete

    let bind (f: 'a -> Arg<'b>) ((px,cx): Arg<'a>) : Arg<'b> =
        let parse tokens =
            match px tokens with
            | Ok x, restx, usagex ->
                    let pf, cf = f x
                    let y, resty, usagey = pf restx
                    y, resty,  { Path = usagey.Path @ usagex.Path; Options = usagey.Options}
            | Error ex, restx, usagex ->
                Error ex, restx, usagex
        let complete pos tokens =
            match px tokens with
            | Ok x, restx, usagex ->
                let pf, cf = f x
                cf pos restx
            | Error _, _, _ ->
                cx pos tokens
        parse, complete
    let ret (x: 'a) : Arg<'a> =
        let parse tokens =
            Ok x, tokens, Usages.empty
        let complete _ _ = [], false
        parse, complete

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

    let alt ((py,cy): Arg<_>) ((px,cx): Arg<_>) : Arg<_> =
        let parse tokens =
            match px tokens, py tokens with
            | (Ok x, restx, usagex), (_, _, usagey) -> Ok x, restx, {  usagex with Options = usagex.Options @ usagey.Options }
            | (_,_,usagex), (Ok y, resty, usagey) -> Ok y, resty, { usagey with Options = usagex.Options @ usagey.Options }
            | (Error ex,_,usagex), (Error ey, _, usagey) -> Error ey, tokens, {Path = []; Options = usagex.Options @ usagey.Options }
        let complete pos tokens =
            let (cpx, ix) = cx pos tokens
            let (cpy, iy) = cy pos tokens
            match ix,iy with
            | false, false -> cpx @ cpy, false
            | true, true -> cpx @ cpy, true
            | true, false -> cpx, true
            | false, true -> cpy, true
        parse, complete

    let optAlt ((py,cy): Arg<'a option>) ((px,cx): Arg<'a option>) : Arg<'a option> =
        let parse tokens =
            match px tokens, py tokens with
            | (Ok (Some x), restx, usagex), (_, _, usagey) -> Ok (Some x), restx, {  usagex with Options = usagex.Options @ usagey.Options }
            | (_,_,usagex), (Ok (Some y), resty, usagey) -> Ok (Some y), resty, {  usagey with Options = usagex.Options @ usagey.Options }
            | (Ok None, rest,usagex), (_,_, usagey)
            | (_, _, usagex), ( Ok None, rest, usagey ) -> Ok None, rest, Usages.merge usagex usagey
            | (Error _,_,usagex), (Error ey, _, usagey) -> Error (ey), tokens, Usages.merge usagex usagey
        let complete pos tokens =
            let (cpx, ix) = cx pos tokens
            let (cpy, iy) = cy pos tokens
            match ix,iy with
            | false, false -> cpx @ cpy, false
            | true, true -> cpx @ cpy, true
            | true, false -> cpx, true
            | false, true -> cpy, true
        parse, complete

    let error message : Arg<_> =
        let parse tokens =
            Error [message], tokens, Usages.empty 
        let complete _ _ = [], false 
        parse, complete
    let errors messages : Arg<_> =
        let parse tokens =
            Error messages, tokens, Usages.empty 
        let complete _ _ = [], false 
        parse, complete      

    let errorf<'t> messageFunc : Arg<'t> =
        let parse tokens =
            let msg = messageFunc tokens
            Error [msg], tokens, Usages.empty
        let complete _ _ = [], false 
        parse, complete      
    let cmdError<'t> : Arg<'t> =
        errorf (function [] -> "Missing command"| token :: _ -> $"Unknown command %s{token.Text}")

    let help text ((p,c): Arg<'t>) : Arg<'t> =
        let parse tokens =
            let result, rest, usages = p tokens
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
        parse, c


module Operators =
    let (<|>) x y = x |> alt y
    let (<|?>) x y = x |> optAlt y
    let (|>>) x v = x |> map (fun _ -> v) 

module Completer =
    let empty (s: string) : string list = []

    let choices (choices: string list) (s: string) _ =
        [ for c in choices do
            if c.StartsWith(s) then
                c
        ]

[<AutoOpen>]
module Run =
    open Core
    open Fargo
    open Operators
    open System.Threading
    open System.Threading.Tasks

    let toResult ((p,c): Arg<'a>) : Arg<ParseResult<'a> * Usages> =
        let parse tokens =
            let result, tokens, usages = p tokens
            Ok (result, usages), tokens, usages
        parse, c
    
    let tryParseTokens ((p,c): Arg<'a>) tokens =
        match p tokens with
        | Ok x, [], _ -> Ok x
        | Error e, _, usages -> Error (e, usages)
        | Ok _, cmd :: _, usages ->
            if cmd.Text.StartsWith("-") then
                Error ([$"Unknown argument %s{cmd.Text}"], usages)
            else
                Error ([$"Unknown command %s{cmd.Text}"], usages)

    let complete ((p,c): Arg<_>) (pos: int) tokens =
        let cp, _ = c pos tokens
        cp

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

    type Shell = Powershell

    let printCompletion appName =
        function
        | Powershell -> 
            printfn """
Register-ArgumentCompleter -Native  -CommandName %s -ScriptBlock {
    param($commandName, $wordToComplete, $cursorPosition)
        %s complete --position $cursorPosition "$wordToComplete" | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
        }
}        
            """ appName appName
     

    let private (|Int|_|) (input: string) =
        Parsers.Int32.tryParse input

    type TopCmd = CompleteCmd | CompletionCmd | RunCmd
    type Top =
        | TopComplete of position:int * cmdLine: Tokens
        | TopCompletion of Shell
        | TopRun of Tokens
        | TopHelp of Tokens * Usages option

    let topArg =
        cmd "complete" null "returns suggestions for auto completion" |>> CompleteCmd
        <|> (cmd "completion" null "emit shell completion script" |>> CompletionCmd)
        <|> ret RunCmd 

    let pPosition =
        opt "position" "p" "cursor-pos" "the current cursor position"
        |> optParse (Parsers.Int32.tryParse >> Parsers.error "position should be a integer")
        |> optValidate (fun p -> p >= 0) "position should be 0 or greater"
        |> defaultValue Int32.MaxValue

    let pShell =
        let shells =  Map.ofList [ "powershell", Powershell ]
        argc "shell" "the shell for which to emit the script" (Completer.choices (shells |> Map.toList |> List.map fst) )
        |> reqArg
        |> parse (fun shell ->
                    match Map.tryFind shell shells with
                    | Some shell -> Ok shell
                    | None -> Error "unknown shell" )

    let pRemoveAppName name : Arg<unit> =
        let exe = name + ".exe"
        let parse tokens =
            match tokens with
            | head :: tail 
                when String.Equals(head.Text, name, StringComparison.InvariantCultureIgnoreCase) 
                || String.Equals(head.Text, exe, StringComparison.InvariantCultureIgnoreCase) ->
                Ok(), tail, Usages.empty    
            | _ -> Ok(), tokens, Usages.empty
        parse, (fun _ _ -> [],false)

    let getUsages ((p,c): Arg<'a>) : Arg<Result<'a, string list> * Usages> =
        let parse tokens =
            let result, tokens, usages = p tokens 
            match result with
            | Ok r -> Ok(Ok r, usages), tokens, usages
            | Error e -> Ok(Error e, usages), tokens, usages
        parse, c

    type Help<'a> =
        | Run of 'a
        | Default of Tokens
        | Help of Tokens * Usages option

    let runHelp ((p,c): Arg<Result<'a,Tokens>>) : Arg<Help<'a>> =
        let ph,ch = flag "help" "h" "display help"
        let parse tokens =
            let help, tokens, u = ph tokens 
            match help with
            | Ok true ->
                let r, tokens', usages = p tokens                
                match r with 
                | Ok (Ok _) ->
                    // this is a command handled by pTop
                    Ok (Help(tokens', Some usages)), [], usages
                | Ok (Error tokens) ->
                    Ok (Help(tokens, None) ), [], usages
                | _ ->
                    Ok(Help (tokens', Some usages)), [], usages

            | Ok false ->
                let r, tokens, usages = p tokens
                match r with
                | Ok (Ok v) -> Ok(Run v), tokens, usages
                | Ok (Error tokens) -> Ok(Default tokens), [], usages
                | Error e -> Error e, tokens, usages 
            |  Error e ->
                Error e, tokens, u
        let complete pos tokens =
            let cph, ih = ch pos tokens
            let cp, i = c pos tokens
            match ih,i with
            | true, true -> cph@cp, true
            | false, false -> cph@cp, false
            | true, false -> cph, true
            | false, true -> cp, false
        parse, complete





    let pTop =
        fargo {
            match! topArg with
            | CompleteCmd ->

                let! position = pPosition
                and! rest = all "..." "the command line text to complete"


                return Ok(TopComplete(position, rest))

            | CompletionCmd ->
                let! shell = pShell
                return Ok (TopCompletion shell)

            | RunCmd ->
                let! tokens = all "..."  ""
                return Error tokens
        }

    let pRun appName =
            fargo {
                do! pRemoveAppName appName
                match! runHelp pTop with
                | Help(rest, usages) ->
                    return TopHelp(rest,usages)
                | Run cmd -> return cmd
                | Default tokens -> return TopRun tokens
            }

    let innerRun (arg: Arg<'a>) tokens (f: 'a -> Task<int>) : Task<int> =
        task {
            match tryParseTokens arg tokens with
            | Ok cmd ->
                try
                    return! f cmd
                with
                | ex ->
                    printErrors [string ex]
                    return -1
            | Error(errors, usages) -> 
                    printErrors errors
                    printfn ""
                    printHelp usages
                    return 0
        }


    let run appName ((p,c): Arg<'a>) (cmdLine: string[]) (f: CancellationToken ->'a  -> Task<int>) : int =
        use cts = new CancellationTokenSource()
        let mutable graceful = true
        Console.CancelKeyPress
        |> Event.add(fun e -> 
            if graceful then
                printfn $"{Colors.yellow}[Ctrl+C]Stopping gracefully. Press Ctrl+C again to force stop.{Colors.def}"
                graceful <- false
                e.Cancel <- true
                cts.Cancel()
            else
                printfn $"{Colors.red}[Ctrl+C]Force stop{Colors.def}"
            )
        let tokens = Token.ofCmdLine cmdLine
        let runner = 
            innerRun (pRun appName) tokens (fun cmd ->
                task {
                    match cmd with
                    | TopComplete(pos, rest)  ->
                        let cmdTokens = 
                            match rest with
                            | [x] -> Token.ofString x.Text
                            | _ -> rest
                        for result in complete (fargo { do! pRemoveAppName appName
                                                        return! (p,c)}) pos cmdTokens do
                            printfn "%s" result  
                        return 0
                    | TopCompletion shell ->
                        printCompletion appName shell
                        return 0
                    | TopRun tokens ->
                        return! innerRun (p,c) tokens (f cts.Token)
                    | TopHelp(_, Some usages) ->
                        printHelp usages
                        return 0
                    | TopHelp(rest, None) ->
                        let _,_,usages = p rest
                        printHelp usages
                        return 0 
                }
        )
        runner.Result





module Pipe =
    open FSharp.Core.CompilerServices
    
    let stdIn : Arg<string list> =
        let parse tokens =
            if Console.IsInputRedirected then
                let mutable values = ListCollector()
                let mutable line = Console.In.ReadLine()
                while line <> null do
                    values.Add(line)
                    line <- Console.In.ReadLine()
                Ok (values.Close()), tokens, Usages.empty
            else
                Ok [], tokens, Usages.empty 
        let complete _ _ = [], false
        parse, complete

    let orStdIn ((p,c): Arg<string option>) : Arg<string list> =
        let parse  tokens =
            if Console.IsInputRedirected then
                let mutable values = ListCollector()
                let mutable line = Console.In.ReadLine()
                while line <> null do
                    values.Add(line)
                    line <- Console.In.ReadLine()
                Ok (values.Close()), tokens, Usages.empty
            else
                let result, tokens, usages = p tokens
                match result with
                | Ok (Some v) -> Ok [v], tokens, usages
                | Ok None -> Ok [], tokens, usages
                | Error e -> Error e, tokens, usages
        parse, c

module Env =
    let envVar name : Arg<string option> =
        let parse tokens =
                match Environment.GetEnvironmentVariable(name) with
                | null -> Ok None, tokens, Usages.empty
                | value -> Ok (Some value), tokens, Usages.empty
        let complete _ _ = [], false
        parse, complete
                



        
