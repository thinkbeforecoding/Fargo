module Fargo

open System

type Quotes =
    | NoQuotes
    | Quotes of char
    | StartQuote of char

type Token =
    { Text: string
      Start: int
      End: int
      Quotes: Quotes }

module Token =
    open Microsoft.FSharp.Core.CompilerServices
    let rec private loop (input: string) (pos: int) (result: ListCollector<_> byref) =
            if pos >= input.Length then
                result.Close()
            else
                if input[pos] = '"' then
                    loopQuote '"' input pos &result 
                elif input[pos] = '\'' then
                    loopQuote '\'' input pos &result 
                else
                    match input.IndexOf(' ', pos) with
                    | -1 ->
                        result.Add { Text = input.Substring(pos)
                                     Start = pos
                                     End = input.Length
                                     Quotes = NoQuotes}
                        result.Close()
                    | n -> 
                        let txt = input.Substring(pos, n-pos)
                        if txt <> "" then
                            result.Add { Text = txt
                                         Start = pos
                                         End = n
                                         Quotes = NoQuotes }
                        loop input (n+1) &result
    and loopQuote quote (input: string) (pos: int) (result: ListCollector<_> byref) =
            match input.IndexOf(quote, pos+1) with
            | -1 ->
                result.Add { Text = input.Substring(pos+1)
                             Start = pos+1
                             End = input.Length
                             Quotes = StartQuote quote}
                result.Close()
            | n -> 
                let txt = input.Substring(pos+1, n-pos-1)
                if txt <> "" then
                    result.Add { Text = txt
                                 Start = pos+1
                                 End = n
                                 Quotes = Quotes quote }
                loop input (n+1) &result

    let ofString (input: string) =
        if isNull input then
            []
        else
            let mutable result = ListCollector()
            loop input 0 &result

    let ofList (input: string list) =
        let mutable result = ListCollector()
        let mutable pos = 0
        for token in input do
            if not (isNull token) then
                let quotes = 
                    if token.IndexOfAny([| ' ';'\'' |]) >= 0 then
                        Quotes '"'
                    elif token.Contains('"') then
                        Quotes '\''
                    else
                        NoQuotes
                result.Add({Text = token; Start = pos; End = pos + token.Length; Quotes = quotes} )
                pos <- pos + token.Length + 1

        result.Close()

    let ofCmdLine (input: string seq) =
        match List.ofSeq input with
        | [singleWord] -> ofString singleWord
        | list -> ofList list

    let toString (tokens: Token list) =
        let builder = Text.StringBuilder()
        let rec loop pos tokens =
            match tokens with
            | token :: rest ->
                let startPos =
                    match token.Quotes with
                    | NoQuotes -> token.Start
                    | _ -> token.Start-1
                if startPos>pos then
                    builder.Append(' ', startPos-pos) |> ignore

                match token.Quotes with
                | Quotes q | StartQuote q -> builder.Append(q) |> ignore
                | _ -> ()
                builder.Append(token.Text) |> ignore
                let endPos =
                    match token.Quotes with
                    | Quotes q ->
                        builder.Append(q) |> ignore
                        token.End+1
                    | _ -> token.End
                    

                loop endPos rest
            | [] -> builder.ToString()
        loop 0 (tokens |> List.sortBy (fun t -> t.Start))


type Usage = { Name: string; Alt: string option; Description: string}
type Usages = Usage list
type CommandLine = Token list

type ParseResult<'t> =
    | Success of 't
    | Failure of string list
    | Complete of string list



type Arg<'a> = int voption -> CommandLine -> ParseResult<'a> * CommandLine * Usages

module Usage =
    let isMatch usage token =
        token.Text = usage.Name ||  (match usage.Alt with Some alt -> token.Text = alt | _ -> false)

    let isStop pos usage token =
        match pos with
        | ValueSome pos ->
            pos >= token.Start && pos <= token.End
        | ValueNone -> false
    
    let (|IsPrefix|_|) pos usage token =
        if isStop pos usage token then
            Some()
        else
            None

    let complete usage input =
        Complete
            [ if usage.Name.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                usage.Name
              match usage.Alt with
              | Some alt ->
                    if alt.StartsWith(input, StringComparison.InvariantCultureIgnoreCase) then
                        alt
              | None -> () ]

    let change f usages =
        match usages with
        | usage :: tail ->
            { Name = usage.Name
              Alt = usage.Alt
              Description = f usage.Description }
            :: tail
        | _ -> usages

module Alt =
    let ofString (name: string) =
        name
        |> Option.ofObj
        |> Option.map ((+) "-")




let cmd name alt description: Arg<_> =
    let usage = { Name = name; Alt = Option.ofObj alt; Description = description}
    fun pos tokens ->
        match tokens with
        | (Usage.IsPrefix pos usage & cmd) :: rest ->
            Usage.complete usage cmd.Text, tokens, [usage]
        | cmd :: tail when Usage.isMatch usage cmd ->
            Success name, tail, [usage]
        | [] ->
            match pos with
            | ValueSome _ -> 
                Usage.complete usage "", tokens, [usage]
            | _ ->
                Failure [$"Command {name} not found"], tokens, [usage]
            
        | _ ->
             Failure [$"Command {name} not found"], tokens, [usage]

let arg name alt description: Arg<_> =
    let usage = { Name = "--" + name; Alt = Alt.ofString alt; Description = description}
    let rec findArg pos tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isStop pos usage x then
                Usage.complete usage x.Text , remaining @ tokens, [usage]
            elif Usage.isMatch usage x  then
                match pos with
                | ValueSome pos ->
                    if pos <= y.End then
                        Complete [], remaining @ tail, [usage]
                    else
                        Success (Some y.Text), remaining @ tail, [usage]

                | ValueNone -> Success (Some y.Text), remaining @ tail, [usage]
            else
                findArg pos rest (remaining @ [x])
        | [x]  ->
            if Usage.isStop pos usage x then
                Usage.complete usage x.Text , remaining @ tokens, [usage]
            elif Usage.isMatch usage x then
                match pos with
                | ValueSome pos ->
                    Complete [], remaining , [ usage]
                | ValueNone -> Failure [$"Argument {usage.Name} value is missing"], remaining, [ usage ] 
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
    fun pos tokens -> findArg pos tokens []

let completer complete (arg: Arg<_ option>) : Arg<_> =
    fun pos tokens ->
        let result, rest, usages = arg pos tokens
        match usages with
        | usage :: _ -> 
            let rec findArg pos tokens remaining =
                match tokens with
                | x :: ((y :: tail) as rest) ->
                    if Usage.isStop pos usage x then
                        Usage.complete usage x.Text , remaining @ tokens, [usage]
                    elif Usage.isMatch usage x  then
                        match pos with
                        | ValueSome pos ->
                            if pos <= y.End then
                                Complete (complete y.Text), remaining @ tail, [usage]
                            else
                                Success (Some y.Text), remaining @ tail, [usage]

                        | ValueNone -> Success (Some y.Text), remaining @ tail, [usage]
                    else
                        findArg pos rest (remaining @ [x])
                | [x]  ->
                    if Usage.isStop pos usage x then
                        Usage.complete usage x.Text , remaining @ tokens, [usage]
                    elif Usage.isMatch usage x then
                        match pos with
                        | ValueSome pos ->
                            Complete (complete ""), remaining , [ usage]
                        | ValueNone -> Failure [$"Argument {usage.Name} value is missing"], remaining, [ usage ] 
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
            findArg pos tokens []
        | _ -> result, tokens, usages


let reqArg (arg: Arg<_>) : Arg<_> =
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
                Failure [$"Required argument {name} not found"]
            | Failure e -> Failure e
            | Complete c -> Complete c
        reqResult, rest, Usage.change reqUsage usages

let flag name alt description =
    let usage = {Name = "--" + name; Alt = Alt.ofString alt; Description = description}
    let rec findFlag pos tokens remaining =
        match tokens with
        | x :: rest ->
            if Usage.isStop pos usage x then
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

let reqFlag (f: Arg<bool>) =
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
                    
                Failure [ $"Required flag {name} not found"]
            | Failure e -> Failure e
            | Complete c -> Complete c
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
        | Complete c, rest, usage -> Complete c, rest, usage

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
        | Complete c, rest, usage ->
            Complete c, rest, usage





let map f (arg: Arg<_>) : Arg<_> =
    fun pos tokens ->
        match arg pos tokens with
        | Success x, rest, usage-> Success (f x), rest, usage
        | Failure e, rest, usage -> Failure e, rest, usage 
        | Complete c, rest, usage -> Complete c , rest, usage 

let optMap f arg = map (Option.map f) arg

let defaultValue d (arg: _ Arg) : _ Arg =
        fun pos tokens ->
            match arg pos tokens with
            | Success None, rest, usage -> Success d, rest, usage
            | Success (Some v), rest, usage -> Success v, rest, usage
            | Failure e, rest, usage -> Failure e, tokens, usage
            | Complete c, rest, usage -> Complete c, rest, usage

let map2 f (argx: Arg<_>) (argy:Arg<_>) : Arg<_> =
        fun pos tokens ->
            match argx pos tokens with
            | Success x, restx, usagex -> 
                match argy pos restx with
                | Success y, resty, usagey -> Success (f x y), resty, usagex @ usagey
                | Failure ey, resty, usagey -> Failure ey, resty, usagex @ usagey
                | Complete cy, resty, usagey -> Complete cy, resty, usagex @ usagey
            | Failure ex, restx, usagex -> 
                match argy pos restx with
                | Success y, resty, usagey -> Failure ex, resty, usagex @ usagey
                | Failure ey, resty, usagey -> Failure (ex@ey), resty, usagex @ usagey
                | Complete cy, resty, usagey -> Complete cy, resty, usagex @ usagey
            | Complete cx, restx, usagex ->
                match argy pos restx with
                | Success y, resty, usagey -> Complete cx, restx, usagex @ usagey
                | Failure ey, resty, usagey -> Complete cx, restx, usagex @ usagey
                | Complete cy, resty, usagey -> Complete (cx @ cy), resty, usagex @ usagey

let bind (f: 'a -> Arg<'b>) (x: Arg<'a>) : Arg<'b> =
    fun pos tokens ->
        match x pos tokens with
        | Success x, restx, usagex ->
                let y, resty, usagey = f x pos restx
                y, resty,  usagey
        | Failure ex, restx, usagex ->
            Failure ex, restx, usagex
        | Complete c, restx, usagex ->
            Complete c, restx, usagex

let ret x : Arg<_> =
    fun pos tokens ->
        Success x, tokens, []

type CmdLineBuilder() =
    member _.Bind(x,f) = bind f x

    member _.BindReturn(x : Arg<'a>,f: 'a -> 'b) : Arg<'b> = 
        map f x

    member _.MergeSources(x : Arg<'a>,y: Arg<'b>) : Arg<'a * 'b> = 
        map2 (fun x y -> x,y) x y

    member _.Return(x) = ret x
    member _.ReturnFrom(x: Arg<'a>) = x

    member _.Zero() = ret ()

let cmdLine = CmdLineBuilder()

let alt (argy: Arg<_>) (argx: Arg<_>) : Arg<_> =
    fun pos tokens ->
        match argx pos tokens, argy pos tokens with
        | (Complete cx , restx, usagex), (Complete cy, _,_) -> Complete (cx @ cy) , restx, usagex
        | (Complete cx , restx, usagex), _ -> Complete cx , restx, usagex
        | _, (Complete cy , resty, usagey) -> Complete cy, resty, usagey
        | (Success x, restx, usagex), (_, _, usagey) -> Success x, restx, usagex@usagey
        | (_,_,usagex), (Success y, resty, usagey) -> Success y, resty, usagex@usagey
        | (Failure ex,_,usagex), (Failure ey, _, usagey) -> Failure (ey), tokens, usagex@usagey

let error msg : Arg<_> =
    fun pos tokens ->
        Failure [msg], tokens, [] 

let errorf fmsg : Arg<_> =
    fun pos tokens ->
        let msg = fmsg tokens
        Failure [msg], tokens, [] 

let cmdError arg =
    errorf (function [] -> "Missing command"| token :: _ -> $"Unknown command {token.Text}") arg

module Int32 =
    let tryParse error (input: string) =
        match Int32.TryParse(input, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error error
module DateTime =
    let tryParse error (input: string) =
        match DateTime.TryParse(input, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AssumeUniversal) with
        | true, v -> Ok v
        | false, _ -> Error error


let tryParse (arg: Arg<_>) tokens =
    let (|Help|_|) tokens = 
        if List.exists (fun t -> t.Text = "--help") tokens then
            Some()
        else
            None

    match arg ValueNone tokens with
    | _, Help, usage -> Error([], usage)
    | Success x, [], _ -> Ok x
    | Success _, cmd :: _, usage -> Error ([$"Unknown command line argument '{cmd}'"], usage)
    | Failure e, _, usage -> Error (e, usage)
    | Complete _, _, usage -> Error ([ "Unexpected completion" ], usage)

let complete (arg: Arg<_>) (pos: int) tokens =

    match arg (ValueSome pos) tokens with
    | Complete choices, _,_ ->
        choices
    |  _,_,_ -> []
let (|Int|_|) (input: string) =
    match Int32.TryParse(input, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | false,_ -> None

module Colors =
    let esc = "\x1B"
    let color n = $"{esc}[%d{n}m"
    let def = color 0
    let red = color 31
    let yellow = color 33

let printErrors errs =
    for err in errs do
        eprintfn $"{Colors.red}{err}{Colors.def}"

let printUsage usage =
    printfn $"{Colors.yellow}Usage:" 
    for u in usage do
        let cmd =
            match u.Alt with
            | None -> u.Name
            | Some alt -> u.Name + ", " + alt
        printfn $"\t%-24s{cmd}%s{u.Description}"
    printfn $"{Colors.def}"


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
            match tryParse arg (Token.ofCmdLine cmdLine) with
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

    let choices (cs: string list) (s: string) =
        [ for c in cs do
            if c.StartsWith(s) then
                c
        ]

module Opertators =
    let (<|>) x y = x |> alt y
    let (|>>) x v = x |> map (fun _ -> v) 
