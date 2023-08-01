# Fargo

A reflectionless command line argument parser with integrated auto completion.

Fargo features:

* An functorial, applicative, monadic and alternative style to avoid reflection
* Reflection free, Fargo can be used with AOT
* Typed, the parsed data is validated
* Error reporting
* Usage and help auto generation
* Extensible auto-completion

## Getting started

Fargo is distributed as a nuget [Fargo.CmdLine](https://www.nuget.org/packages/Fargo.CmdLine):

```pwsh
dotnet add package Fargo.CmdLine
```

In a fsx script:
```fsharp
#r "nuget: Fargo.CmdLine"
```

You can try it with this simple hello world. Create a hello project:

```pwsh
dotnet new console -lang F# -o ./hello
cd ./hello
dotnet add package Fargo.CmdLine
```

and edit the `Program.fs` file:

```fsharp
open Fargo

let parser = opt "text" "t" "text" "The text to display" |> reqOpt

[<EntryPoint>]
let main args =
    run "hello" parser args (fun ct text ->
        task { printfn "%s" text; return 0; })
```

build the project
```pwsh
dotnet build -c Release
```

and add the output path to the `PATH` environment variable:
```pwsh
$p = Resolve-Path .\bin\Release\net7.0\
$env:PATH += ";$p"
```

Run the program
```pwsh
hello --text "Hello world!"
```

To display help:
```pwsh
hello --help
```
will display:
```
Usage: --text <text> 
Arguments:
    --text, -t <text>       The text to display
```

If arguments are missing or incorrect, a detailed error is returned:
```pwsh
hello
```
```
hello
Required argument --text not found

Usage: --text <text>
Arguments:
    --text, -t <text>       The text to display
```

To enable completion in powershell, execute the following line:
```
hello completion powershell | out-string | invoke-expression
```
This line can be added to the $Profile.CurrentUserAllHost file to enable completion for every new session.

Now, type `hello` followed by a space, and press tab or Ctrl+Space. The arguments are suggested.

## Arg<'t>

`Arg<'t>` is the type of a command line parser in Fargo.

## Flag

Flags are declared using the `flag` function:

```fsharp
flag "force" "f" "force the copy" // Arg<bool>
```

this creates a `--force` flag with a `-f` shortcut.

A flag creates an `Arg<bool>` with a value of `Success true` when specified and a value of `Success false` otherwise.

When the second argument is `null` the flag has no short alternate version.

## ReqFlag

A flag can be made mandatory by using the `reqFlag`function:

```fsharp
flag "force" "f" "force the copy" |> reqFlag // Arg<bool>
```

It creates an `Arg<bool>` with a value of `Success true` when specified and a value of `Failure ["Required flag --force not found"]` otherwise

## Arg

Arguments accept a positional value, and are optional by default:

```fsharp
arg "value" "the value" // Arg<string option>
```

Arguments are positional contrary to [options](#opt) which are named

It creates an `Arg<string option>` with a value of `Success(Some "a value")` containing the value when specified, and `Success None` otherwise.

When the second argument is `null` the flag has no short alternate version.

## ReqArg

To make an argument required, use the `reqArg` function:

```fsharp
arg "value" "the value" |> reqArg |> Arg<string>
```

It creates an `Arg<string>` with a value of `Success "a value"` containing the value when specified, and `Failure ["Require argument --value not found"]` otherwise.

The first parameter is used as a placeholder in the usage syntax.


## Opt

Options accept a named value, and are optional by default:

```fsharp
opt "name" "n" "the-name" "the name" // Arg<string option>
```

It creates an `Arg<string option>` with a value of `Success(Some "a value")` containing the value when specified, and `Success None` otherwise.

When the second argument is `null` the flag has no short alternate version.

The third parameter is used as a placeholder in the usage syntax.

## ReqArg

To make an argument required, use the `reqArg` function:

```fsharp
opt "name" "n" "the-name" "the name" |> reqOpt |> Arg<string>
```

It creates an `Arg<string>` with a value of `Success "a value"` containing the value when specified, and `Failure ["Require argument --value not found"]` otherwise.

## Map

To change the type of the return value of any `Arg<'t>`, use the `map` function:
```fsharp
map: ('a -> 'b) -> Arg<'a> -> Arg<'b>
```

It works best with required arguments:
```fsharp
arg "value" "v" "the value" |> reqArg |> map int
```
This returns an `Arg<int>` that contains the argument value converted to `int`. However, any input that cannot be converted to string will throw an exception. To avoid this, use `parse`.

### OptMap

For optional arguments, use `optMap`:
```fsharp
optMap: ('a -> 'b) -> Arg<'a option> -> Arg<'b option>
```
The function is applied to the value inside the option. As for `map` a problem with the conversion will raise an exception. To avoid this, use `optParse`.

### Parse

```fsharp
parse ('a -> Result<'b, string>) -> Arg<'a> -> Arg<'b>
```

The function passed to the `parse` functions returns a `Result<'b,string>` that can represent a success or an failure with the error message. In case of error, the parser result will contain the error message.

```fsharp
open System
open System.Globalization
let tryParseInt (input: string) =
    match Int32.TryParse(input, CultureInfo.InvariantCulture) with
    | true, value -> Ok value
    | false, _ -> Error "Input value is not an integer"

arg "value" "v" "the value" |> reqArg |> parse tryParseInt
```

Instead of throwing an exception, a parsing failure will be returned. This is especially important for [Alternatives](#Alternatives)

### OptParse

This function is equivalent to `parse` for optional arguments:

```fsharp
optParse ('a -> Result<'b, string>) -> Arg<'a option> -> Arg<'b option>
```

```fsharp
arg "value" "v" "the value" |> optParse tryParseInt // Arg<int option>
```

### DefaultValue

This function is used to specify a default value for optional arguments:
```fsharp
defaultValue: 'a -> Arg<'a option> -> Arg<'a>
```


```fsharp
arg "value" "v" "the value"
|> optParse tryParseInt
|> defaultValue 0 // Arg<int>
```

In this example, when the argument is not specified, the value will be 0. When specified, it will be an int if it can be correctly parsed, or return an error otherwise.



### Completer

A custom completer can be specified to enable completion on argument values:

```fsharp
argc "value" "the value" (Completer.choices ["one"; "two"; "three" ])
optc "name" "n" "value" "the name" (Completer.choices ["a"; "b"; "c" ])
```

Pressing the `tab` key after `--name` will suggest one of the specified values.

The function passed must have the following signature:
```fsharp
string -> Token list -> string list
```
The input string is the text of the argument value when completion is requested. The token list contains all tokens that have not been parsed yet. The function should return a list of suggested values as strings.

## Map2 and Applicatives

The `map2` function can be used to combine two arguments together:
```fsharp
map2: ('a -> 'b -> 'c) -> Arg<'a> -> Arg<'b> -> Arg<'c>
```
The specified function is used to combine the values of the two passed arguments. The result is an argument with combined results:
```fsharp
map2 (fun firstName lastName -> firstName + " " + lastName)
     (arg "first-name" "f" "The user firstname" |> reqArg)
     (arg "last-name" "l" "The user last name" |> reqArg)
     // Arg<string>
```

Is is clearer to use the applicative computation expression `cmdLine` instead:
```fsharp
cmdLine {
    let! firstName = arg "first-name" "f" "The user firstname" |> reqArg
    and! lastName = arg "last-name" "l" "The user last name" |> reqArg
    return firstName + " " + lastName
}    // Arg<string>
```
It is of course possible to combine values in different ways, especially in a tuple or a record, and to bind more values using `and!` :
```fsharp
type User = 
    { FirstName: string
      LastName string
      Age: int option }

cmdLine {
    let! firstName = arg "first-name" "f" "The user firstname" |> reqArg
    and! lastName = arg "last-name" "l" "The user last name" |> reqArg
    and! age = arg "age" "a" "The user age" |> optParse tryParseInt
    return { FirstName = firstName
             LastName = lastName
             Age = age }
}   // Arg<User>
```

### Cmd

The `cmd` function create a command:
```fsharp
cmd "load" "ld" "Loads the document" // Arg<string>
```
Contrary to flags and args which can be matched at any position, a command is always matched in the first position. Its value is the name of the command itself. In the example above, the value will be `"load"` even if the alternate short version is used.

When the second argument is `null`, the command has no short alternate version.

## Alternatives

The `<|>` operator can be used to combine two parsers together. This is especially useful for commands:

```fsharp
(cmd "load" "ld" "loads the document")
<|> (cmd "save" "sv" "saves the document")
    // Arg<string>
```

If the command on the left matches, its value is returned. Otherwise, the second command is tested.

The `<|>` can be used multiple times to combine more commands.

```fsharp
type Cmd = Load | Save | Delete
(cmd "load" "ld" "loads the document" |~> Load)
<|> (cmd "save" "sv" "saves the document" |~> Save)
<|> (cmd "delete" "del" "deletes the document" |~> Delete)
<|> (error "Invalid file command") 

// Arg<Cmd>
```

Here, for each command the `|~>` operator is used to replace the original `string` value, which is the name of the command, with the supplied value.

The `error` function creates an `Arg<'t>` that always fails with specified error message. It will be used only if all of the commands above fail, displaying a specific error message.

## Bind and Monads

```fsharp
bind: ('a -> Arg<'b>) -> Arg<'a> -> Arg<'b>
```

The use of `bind` directly is discouraged, prefer the computation expression:

```fsharp
type FileCmd = Load | Save
type Command =
| Load of string
| Save of string * bool
cmdLine {
    match! (cmd "load" "ld" "loads the document" |~> FileCmd.Load)
           <|> (cmd "save" "sv" "saves the document" |~> FileCmd.Save)
           <|> (error "Invalid file command")  with
    | FileCmd.Load ->
        let! path = opt "path" "p" "path" "the path"
        return Load path
    | FileCmd.Load ->
        let! path = opt "path" "p" "path" "the path"
        and! force = flag "force" "f" "overwrite file"
        return Save(path, force)
} // Arg<Command>
```
Using a `let!` (`match!` is a shortcut for a `let!` followed by a `match`) followed by another `let!` combine them as nested levels. If the match of the command fails, the usage will display commands from the alternative. If it succeeds, the usage will display the arguments of the returned cases.

Since commands return their name, it is possible to match directly on it:

```fsharp
type Command =
| Load of string
| Save of string * bool
cmdLine {
    match! cmd "load" "ld" "loads the document"
           <|> cmd "save" "sv" "saves the document" with
    | "load" -> 
        let! path = opt "path" "p" "path" "the path"
        return Load path
    | "save" ->
        let! path = opt "path" "p" "path" "the path"
        and! force = flag "force" "f" "overwrite file"
        return Save(path, force)
    | _ -> return error "Unknown command"
} // Arg<Command>
```

To define a default command, just handle it in the default case:
```fsharp
type Command =
| Load of string
| Save of string * bool
| Touch of string
cmdLine {
    match! cmd "load" "ld" "loads the document"
           <|> cmd "save" "sv" "saves the document" with
    | "load" -> 
        let! path = opt "path" "p" "path" "the path"
        return Load path
    | "save" ->
        let! path = opt "path" "p" "path" "the path"
        and! force = flag "force" "f" "overwrite file"
        return Save(path, force)
    | _ ->
        let! path = opt "path" "p" "path" "the path"
        return Touch path
} // Arg<Command>
```

Using another nested level of `let!` or `match!` it is possible to create sub-commands. 

## Ret

The `ret` function can be used to return a constant value.
It can also be used for default command:
```fsharp
type FileCmd = Load | Save | Touch
type Command =
| Load of string
| Save of string * bool
| Touch of string
cmdLine {
    match! (cmd "load" "ld" "loads the document" |~> FileCmd.Load)
           <|> (cmd "save" "sv" "saves the document" |~> FileCmd.Save)
           <|> (ret FileCmd.Touch)  with
    | FileCmd.Load ->
        let! path = arg "path" "p" "the path"
        return Load path
    | FileCmd.Load ->
        let! path = arg "path" "p" "the path"
        and! force = flag "force" "f" "overwrite file"
        return Save(path, force)
    | FileCmd.Touch ->
        let! path = arg "path" "p" "the path"
        return Touch path}
    // Arg<Command>
```

or to give default value to optional arguments:

```fsharp
arg "value" "v" "the value" <|> optParse tryParseInt <|> ret 0 
```

## Pipe / OrPipe

It is possible to get values from standard input. The `Pipe.pipe` value is an `Arg<string list>` that returns lines read from the standard input. It fails if the input has not been redirected. If the pipe contains no value, it succeeds with an empty list. Use `nonEmpty` to ensure there is at list one element.

To enable a parameter to be specified either directly or from the pipe, use the `Pipe.orPipe` function:

```fsharp
arg "value" "v" "the value"
|> Pipe.orPipe
|> nonEmpty "The required argument --value is missing"
|> listParse (Int32.tryParse "Invalid value")
// Arg<int list>
```

The argument returns a list of values, and fails if no value has been provided, either using the argument or the pipe.

## Run

The `run` function runs a parser:
```fsharp
let p =
   cmdLine { ... }

[<EntryPoint>]
let main (args: string[]) =
    run "myapp" p args (fun ct cmd ->
        task {
            // excution of match commands here...
            return 0
        }
    )
```

The first argument of the `run` function is the name of the application. It is used in the usage, and to generate the shell completion script.

The second argument is the parser, followed by the application argument array. Finally, the last argument is a function that use the result of the parsing to execute the commands.

`run` takes in charge:
* the parsing
* displaying the error returned by the parser
* showing usage on error, or when the --help flag is used
* the tab completion
* emitting code for shell tab completion integration
