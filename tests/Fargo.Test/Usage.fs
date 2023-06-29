module Fargo.Usage

open System
open System.Text.RegularExpressions
open Fargo
open Fargo.Opertators
open Xunit

let (=!) (actual:'a) (expected:'a) = Assert.Equal<'a>(expected, actual)

let outUsage p input =
    let _,_,usages = p ValueNone (Token.ofString input)
    let w = new IO.StringWriter()
    Console.SetOut(w)
    printHelp usages
    Regex.Replace(w.ToString(),$"{'\x1b'}\[\d+m","").Split('\n')
    |> Array.map (fun l -> l.TrimEnd())
    |> Array.filter (not << String.IsNullOrEmpty)
    |> String.concat Environment.NewLine


[<Fact>]
let ``arg usage``() =
    let p = arg "value" "the value"
    outUsage p ""
    =! $"""Usage: [<value>]
Options:
    <value>                 the value"""

[<Fact>]
let ``reqArg usage``() =
    let p = arg "value" "the value" |> reqArg
    outUsage p  ""
    =! $"""Usage: <value>
Arguments:
    <value>                 the value"""

[<Fact>]
let ``opt usage``() =
    let p = opt "opt" "o" "value" "the value"
    outUsage p ""
    =! $"""Usage: [options]
Options:
    --opt, -o <value>       the value"""

[<Fact>]
let ``reqOpt usage``() =
    let p = opt "opt" "o" "value" "the value" |> reqOpt
    outUsage p ""
    =! $"""Usage: --opt <value>
Arguments:
    --opt, -o <value>       the value"""

[<Fact>]
let ``flag usage``() =
    let p = flag "flag" "f" "the value"
    outUsage p ""
    =! $"""Usage: [options]
Options:
    --flag, -f              the value"""

[<Fact>]
let ``reqFlag usage``() =
    let p = flag "flag" "f" "the value" |> reqFlag
    outUsage p ""
    =! $"""Usage: --flag
Arguments:
    --flag, -f              the value"""

[<Fact>]
let ``cmd usage``() =
    let p = cmd "command" "cmd" "the value"
    outUsage p ""
    =! $"""Usage: [command]
Commands:
    command, cmd            the value"""

[<Fact>]
let ``cmd alt usage``() =
    let p = 
        cmd "load" "ld" "load the file"
        <|> cmd "save" "sv" "save the file"
        <|> error "Nope"
    outUsage p ""
    =! $"""Usage: [command]
Commands:
    load, ld                load the file
    save, sv                save the file"""

[<Fact>]
let ``cmd alt usage after matching``() =
    let p =
        fargo {
            let! c = 
                cmd "load" "ld" "load the file"
                <|> cmd "save" "sv" "save the file"
                <|> error "Nope"
            let! o = opt "opt" "o" "value" "the value" 
            return c }
        
    outUsage p "save "
    =! $"""Usage: save [options]
Options:
    --opt, -o <value>       the value"""


[<Fact>]
let ``nest cmd alt usage after matching``() =
    let p =
        fargo {
            match! cmd "file" null "file operation"
                    <|> cmd "other" null "other"
            with
            | "file" ->
                match! 
                    cmd "load" "ld" "load the file"
                    <|> cmd "save" "sv" "save the file"
                with
                | "load" ->
                    let! o = opt "opt" "o" "value" "the value" |> defaultValue "" 
                    return o
                | "save" ->
                    let! o = arg "path" "the path" |> reqArg
                    return o
                | _ -> return! error "Unknown command"
            | _ -> return! error "Unknown command"
             }
        
    outUsage p "file "
    =! $"""Usage: file [command]
Commands:
    load, ld                load the file
    save, sv                save the file"""

[<Fact>]
let ``nest cmd alt usage after matching both commands``() =
    let p =
        fargo {
            match! cmd "file" null "file operation"
                    <|> cmd "other" null "other"
            with
            | "file" ->
                match! 
                    cmd "load" "ld" "load the file"
                    <|> cmd "save" "sv" "save the file"
                with
                | "load" ->
                    let! o = opt "opt" "o" "value" "the value" |> defaultValue "" 
                    return o
                | "save" ->
                    let! o = arg "path" "the path" |> reqArg
                    return o
                | _ -> return! error "Unknown command"
            | _ -> return! error "Unknown command"
             }
        
    outUsage p "file save"
    =! $"""Usage: file save <path>
Arguments:
    <path>                  the path"""

[<Fact>]
let ``applicative``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outUsage p "save "
    =! $"""Usage: [options] <arg>
Arguments:
    <arg>                   the arg
Options:
    --opt, -o <value>       the value
    --flag, -f              the flag"""
