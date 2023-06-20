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

Fargo is distributed as a nuget:

```pwsh
dotnet add package Fargo
```

In a fsx script:
```fsharp
#r "nuget: Fargo
```

You can try it with this simple hello world. Create a hello project:

```pwsh
dotnet new console -lang F# -o ./hello
cd ./hello
dotnet add package Fargo
```

and edit the `Program.fs` file:

```fsharp
open Fargo

let parser = arg "text" "t" "The text to display" |> reqArg

[<EntryPoint>]
let main args =
    run "hello" parser args (fun text -> printfn "%s" text)
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
```psws
hello --text "Hello world!"
```

To display help:
```pwsh
hello --help
```
will display:
```
hello
Required argument --text not found

Usage:
        --text, -t              The text to display (required)
```

If arguments are missing or incorrect, a detailed error is returned:
```pwsh
hello
```
```
hello
Required argument --text not found

Usage:
        --text, -t              The text to display (required)
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
flag "force" "f" "force the copy"
```

this creates a `--force` flag with a `-f` shortcut.

A flag creates an `Arg<bool>` with a value of `Success true` when specified and a value of `Success false` otherwise.

## ReqFlag

A flag can be made mandatory by using the `reqFlag`function:

```fsharp
flag "force" "f" "force the copy" |> reqFlag
```

It creates an `Arg<bool>` with a value of `Success true` when specified and a value of `Failure ["Required flag --force not found"]` otherwise

## Arg

Arguments accept a value, and are optional by default:

```fsharp
arg "value" "v" "the value"
```

It creates an `Arg<string option>` with a value of `Success(Some "a value")` containing the value when specified, and `Success None` otherwise.

## ReqArg

To make an argument required, use the `reqArg` function:

```fsharp
arg "value" "v" "the value" |> reqArg
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
arg "value" "v" "the value" |> optParse tryParseInt
```

### DefaultValue

This function is used to specify a default value for optional arguments:
```fsharp
defaultValue: 'a -> Arg<'a option> -> Arg<'a>
```


```fsharp
arg "value" "v" "the value"
|> optParse tryParseInt
|> defaultValue 0
```

In this example, when the argument is not specified, the value will be 0. When specified, it will be an int if it can be correctly parsed, or return an error otherwise.



### Completer

A custom completer can be specified to enable completion on argument values:

```fsharp
arg "value" "v" "the value" 
|> completer Completer.choices ["one"; "two"; "three" ]
```

Pressing the `tab` key after `--value` will suggest one of the specified values.

The function passed to the `completer` function must have the following signature:
```fsharp
string -> string list
```
The input string is the text of the argument value when completion is requested. The function should return a list of suggested values as strings.

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
```

Is is clearer to use the applicative computation expression `cmdLine` instead:
```fsharp
cmdLine {
    let! firstName = arg "first-name" "f" "The user firstname" |> reqArg
    and! lastName = arg "last-name" "l" "The user last name" |> reqArg
    return firstName + " " + lastName
}    // type is Arg<string>
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
}       // type is Arg<User>
```
## Alternatives

The `<|>` operator can be used to combine two parsers together. This is especially useful for commands:

```fsharp
(cmd "load" "ld" "loads the document")
<|> (cmd "save" "sv" "saves the document")
```

If the command on the left matches, its value is returned. Otherwise, the second command is tested.

The `<|>` can be used multiple times to combine more commands.

```fsharp
type Cmd = Load | Save | Delete
(cmd "load" "ld" "loads the document" |~> Load)
<|> (cmd "save" "sv" "saves the document" |~> Save)
<|> (cmd "delete" "del" "deletes the document" |~> Delete)
<|> (error "Invalid file command") 

// the type is Arg<Cmd>
```

Here, for each command the `|~>` operator is used to replace the original `string` value, which is the name of the command, with the supplied value.

The `error` function creates an `Arg<'t>` that always fails with specified error message. It will be used only if all of the commands above fail, displaying a specific error message.

## Bind and Monads

```fsharp
bind: ('a -> Arg<'b>) -> Arg<'a> -> Arg<'b>
```

The use of `bind` directly is discouraged, prefere the computation expression:

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
        let! path = arg "path" "p" "the path"
        return Load path
    | FileCmd.Load ->
        let! path = arg "path" "p" "the path"
        and! force = flag "force" "f" "overwrite file"
        return Save(path, force)
} // type is Arg<Command>
```
Using a `let!` (`match!` is a shortcut for a `let!` followed by a `match`) followed by another `let!` combine them as nested levels. If the match of the command fails, the usage will display commands from the alternative. If it succeeds, the usage will display the arguments of the returned cases.

Using another nested level of `let!` or `match!` it is possible to create sub-commands. 

## Run

The `run` function runs a parser:
```fsharp
let p =
   cmdLine { ... }

[<EntryPoint>]
let main (args: string[]) =
    run "myapp" p args (fun cmd ->
        // excution of match commands here...
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