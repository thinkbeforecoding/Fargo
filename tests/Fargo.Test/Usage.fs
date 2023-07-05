module Fargo.Usage

open System
open System.Text.RegularExpressions
open Fargo
open Fargo.Operators
open Xunit
open DEdge.Diffract
open System.Threading.Tasks

let (=!) (actual:'a) (expected: 'a) = Differ.Assert(expected, actual )

let outUsage p input =
    let _,_,usages = p ValueNone (Token.ofString input)
    let w = new IO.StringWriter()
    Console.SetOut(w)
    printHelp usages
    Regex.Replace(w.ToString(),$"{'\x1b'}\[\d+m","").Split('\n')
    |> Array.map (fun l -> l.TrimEnd())
    |> Array.filter (not << String.IsNullOrEmpty)
    |> String.concat Environment.NewLine

let outRun p input =
    let w = new IO.StringWriter()
    Console.SetOut(w)
    run "test" p [|input|] (fun _ v -> printfn "%A" v; Task.FromResult 0) |> ignore
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
        <|> (cmd "save" "sv" "save the file" |> help "saves the file to avoid bad things")
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
                <|> (cmd "save" "sv" "save the file" |> help "saves the file to avoid bad things")
                <|> error "Nope"
            let! o = opt "opt" "o" "value" "the value" 
            return c }
        
    outUsage p "save "
    =! $"""Usage: save [options]
saves the file to avoid bad things
Options:
    --opt, -o <value>       the value"""


[<Fact>]
let ``nested cmd alt usage after matching``() =
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
file operation
Commands:
    load, ld                load the file
    save, sv                save the file"""

[<Fact>]
let ``nested cmd alt usage after matching both commands``() =
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
save the file
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

[<Fact>]
let ``help``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "--help"
    =! $"""Usage: [options] <arg>
Arguments:
    <arg>                   the arg
Options:
    --opt, -o <value>       the value
    --flag, -f              the flag"""


[<Fact>]
let ``complete help``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "complete --help"
    =! $"""Usage: complete [options] [<...>]
returns suggestions for auto completion
Options:
    --position, -p <cursor-pos> the current cursor position
    <...>                   the command line text to complete"""


[<Fact>]
let ``completion help``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "completion --help"
    =! $"""Usage: completion <shell>
emit shell completion script
Arguments:
    <shell>                 the shell for which to emit the script"""


[<Fact>]
let ``complettion run``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "completion powershell"
    =! """Register-ArgumentCompleter -Native  -CommandName test -ScriptBlock {
    param($commandName, $wordToComplete, $cursorPosition)
        test complete --position $cursorPosition "$wordToComplete" | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
        }
}"""


[<Fact>]
let ``standard run``() =
    let p =
        fargo {
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "-o x value"
    =! """(Some "x", "value")"""

[<Fact>]
let ``cmd help``() =
    let p =
        fargo {
            let! _ = cmd "cmd" null "a command"
            let! o = opt "opt" "o" "value" "the value"
            and! f = flag "flag" "f" "the flag"
            and! a = arg "arg" "the arg" |> reqArg
            return o,a }
        
    outRun p "cmd --help"
    =! $"""Usage: cmd [options] <arg>
a command
Arguments:
    <arg>                   the arg
Options:
    --opt, -o <value>       the value
    --flag, -f              the flag"""
