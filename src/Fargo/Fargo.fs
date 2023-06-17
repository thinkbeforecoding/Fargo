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


type Usage = { Name: string; Alt: string option; Description: string; Completer: string -> string list}
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
            if pos <= token.End then
                if pos >= token.Start then
                    let tokenStart = token.Text.Substring(0,pos-token.Start)
                    usage.Name.StartsWith(tokenStart)
                    || match usage.Alt with Some alt -> alt.StartsWith(tokenStart) | None -> false
                else
                    true
            else
                false
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

module Alt =
    let ofString (name: string) =
        name
        |> Option.ofObj
        |> Option.map ((+) "-")


let cmd name alt description: Arg<_> =
    let usage = { Name = name; Alt = Option.ofObj alt; Description = description; Completer = fun _ -> [] }
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

let arg name alt description completer: Arg<_> =
    let usage = { Name = "--" + name; Alt = Alt.ofString alt; Description = description; Completer = completer }
    let rec findArg pos tokens remaining =
        match tokens with
        | x :: ((y :: tail) as rest) ->
            if Usage.isStop pos usage x then
                Usage.complete usage x.Text , remaining @ tokens, [usage]
            elif Usage.isMatch usage x  then
                match pos with
                | ValueSome pos ->
                    if pos <= y.End then
                        Complete (usage.Completer y.Text), remaining @ tail, [usage]
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
                    Complete (usage.Completer ""), remaining , [ usage]
                | ValueNone -> Failure [$"Argument {usage.Name} value is missing"], remaining, [ usage ] 
            else
                Success None, remaining @ tokens , [ usage ]
        | [] ->
            match pos with
            | ValueSome pos ->
                Usage.complete usage "" , remaining @ tokens, [usage]
            | ValueNone -> Success None, remaining  , [ usage ]
    fun pos tokens -> findArg pos tokens []

let req name alt description completer: Arg<_> =
    let arg = arg name alt description completer
    fun pos tokens ->
        match arg pos tokens with
        | Success (Some v), rest, usage -> Success v, rest, usage
        | Success None, rest, (usage :: _ as usages) -> Failure [$"Required argument {usage.Name} not found"], rest, usages
        | Success None, _, _ -> failwith "Unexpected parsing error"
        | Failure e, rest, usage -> Failure e, rest, usage
        | Complete c, rest, usage -> Complete c, rest, usage

let flag name alt description =
    let usage = {Name = "--" + name; Alt = Alt.ofString alt; Description = description; Completer = fun _ -> [] }
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

let reqFlag name alt description =
    let f = flag name alt description
    fun pos tokens ->
        match f pos tokens with
        | Success true, rest, usage -> Success true, rest, usage
        | Success false, rest, (usage::_ as usages)  -> Failure [ $"Required flag {usage.Name} not found"], rest, usages
        | Success false, rest, [] -> failwith "Unexpected parsing error"
        | Failure e, rest, usage -> Failure e, rest, usage
        | Complete c, rest, usage -> Complete c, rest, usage

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
        fun pos token ->
            match arg pos token with
            | Success None, rest, usage -> Success d, rest, usage
            | Success (Some v), rest, usage -> Success v, rest, usage
            | Failure e, rest, usage -> Failure e, rest, usage
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



module Int32 =
    let tryParse error (input: string) =
        match Int32.TryParse(input, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error error
module DateTime =
    let tryParse error (input: string) =
        match DateTime.TryParse(input, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error error

// type Cmd = Mv | Del

// type CmdArg =
//     | Move of string  * int option * bool
//     | Delete of string option

// let cmd' = 
//     cmd "move" "mv" "Move things" |> map (fun _ -> Mv)
//     |> alt (cmd "delete" "del" "Delete things" |> map (fun _ -> Del)) 
//     |> alt (error "Invalid command")


// let both =
//     cmdLine {
//         match! cmd' with
//         | Mv ->
//             let! dst = arg "destintation" "d" "the command destination" |> defaultValue "default"
//             and! groupId = arg "group" "g" "The group id" |> optParse (Int32.tryParse "Invalid group id")
//             and! force = flag "force" "f" "force the command"
//             return Move(dst, groupId, force)
//         | Del ->

//             let! thing = arg "thing" "t" "the thing" 
//             return Delete(thing)

//     }

// tryParse both [ "mv"; "-g"; "x123";  "-f"]
    
let (<|>) x y = x |> alt y

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
    | r, rest, usage -> failwith $"{(r,rest,usage)}"
    // | _, rest, usage ->
    //     match rest with
    //     | [] ->
    //         [ for cmd in usage do
    //             cmd.Name
    //             match cmd.Alt with
    //             | Some alt -> alt
    //             | None -> ()
    //         ]
    //     | x :: tail ->
    //         match usage |> List.tryFind (fun u -> Usage.isMatch  u x ) with
    //         | Some u ->
    //             let next = List.tryHead tail |> Option.map (fun t -> t.Text)  |> Option.defaultValue ""
    //             u.Completer next
    //         | None ->
    //             [ for cmd in usage do
    //                 if cmd.Name.StartsWith(x.Text) then
    //                     cmd.Name
    //                 match cmd.Alt with
    //                 | Some alt when alt.StartsWith(x.Text) -> alt
    //                 | _ -> ()
    //             ]
let (|Int|_|) (input: string) =
    match Int32.TryParse(input, Globalization.CultureInfo.InvariantCulture ) with
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

// let pArg =
//     cmdLine {
//         match! cmd "build" null "build" <|> (cmd "publish" "pub" "publish") with
//         | "build" -> 
//             match! cmd "local" null "local" <|> (cmd "remote" null "remote") with
//             | "local" ->
//                 let! path = arg "path" "p"  "path" Completer.empty
//                 and! config = arg "config" "c" "config" (Completer.choices ["release"; "debug"])
//                 return Choice1Of3(path, config)
//             | "remote" ->
//                 let! path = arg "path" "p"  "path" Completer.empty
//                 let! server = arg "server" "s"  "server" Completer.empty
//                 and! config = arg "config" "c" "config" (Completer.choices ["release"; "debug"])
//                 return Choice2Of3(path, server, config)
//         | "publish" ->
//             let! path = arg "path" "p"  "path" Completer.empty
//             and! config = arg "config" "c" "config" (Completer.choices ["release"; "debug"])
//             and! output = arg "output" "o"  "path" Completer.empty
//             return Choice3Of3(path, config, output)
//         | _ -> return failwithf "Nope"
//     }

// complete pArg 6 (Token.ofString "build r")
// complete pArg 12 (Token.ofString "build local ")

// let pPath = (arg "path" "p" "path" Completer.empty)
// let pOut = arg "output" "o" "out" Completer.empty
// let _, tks, u = 
//     (map2 (fun x y -> x,y) pPath pOut) (ValueSome 1) (Token.ofString "-")

// pPath (ValueSome 1) (Token.ofString "-")

// Usage.isStop (ValueSome 1) u[0] tks[0]