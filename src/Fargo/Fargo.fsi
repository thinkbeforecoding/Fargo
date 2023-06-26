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
    /// converts a command line string to a list of tokens
    val ofString:  string -> Token list 
    /// converts a list of command line strings to a list of tokens
    val ofList: string list -> Token list
    /// convert the command line supplied to tokens
    val ofCmdLine: string seq -> Token list
    /// converts a token list to a string (keeping original position)
    val toString: Token list -> string


type Usage = { Name: string; Alt: string option; Description: string}
type Usages = Usage list
type CommandLine = Token list
type Completer = string -> string list

type ParseResult<'t> =
    | Success of 't
    | Failure of string list
    | Complete of string list * important: bool

type Arg<'a> = int voption -> CommandLine -> ParseResult<'a> * CommandLine * Usages

module Usage =
    val isMatch: Usage -> Token -> bool
    val isStop: int voption -> Token -> bool
    
    val (|IsPrefix|_|): int voption -> Token -> unit option

    val complete: Usage -> string -> ParseResult<'t>

    val change: (string -> string) -> Usage list -> Usage list

module Alt =
    val ofString: name:string -> string option


/// parse the command line arguments with the specifier arg parser and pass
/// parsed result to the suppied function
val cmd: name:string -> alt:string -> description:string -> Arg<string>
val arg: name:string -> alt:string -> description:string -> Arg<string option>
val completer: Completer -> Arg<string option> -> Arg<string option>

val reqArg: Arg<'a option> -> Arg<'a>
val flag: string -> string -> string -> Arg<bool>

val reqFlag: Arg<bool> -> Arg<bool>

val parse: ('a -> Result<'b, string>) ->  Arg<'a> -> Arg<'b>
val optParse: ('a -> Result<'b, string>) -> Arg<'a option> -> Arg<'b option>
val listParse: ('a -> Result<'b, string>) -> Arg<'a list> -> Arg<'b list>

val nonEmpty: string -> Arg<'a list> -> Arg<'a list>

val map: ('a -> 'b) -> Arg<'a> -> Arg<'b>
val optMap: ('a -> 'b) -> Arg<'a option> -> Arg<'b option>
val defaultValue: 'a ->  Arg<'a option>  -> Arg<'a>
val map2: ('a -> 'b -> 'c) -> Arg<'a> -> Arg<'b> -> Arg<'c>
val bind: ('a -> Arg<'b>) -> Arg<'a> -> Arg<'b>
val ret: 'a -> Arg<'a>

[<Class>]
type CmdLineBuilder =
    member Bind: Arg<'a> * ('a -> Arg<'b>) -> Arg<'b>
    member BindReturn: Arg<'a> * ('a -> 'b) -> Arg<'b>
    member MergeSources:  Arg<'a> * Arg<'b> -> Arg<'a * 'b>
    member Return: 'a -> Arg<'a>
    member ReturnFrom: Arg<'a> -> Arg<'a>
    member Zero: unit -> Arg<unit>

val cmdLine: CmdLineBuilder

val alt: Arg<'a> -> Arg<'a> -> Arg<'a>
val error: message:string -> Arg<'a>
val errorf<'a> : messageFunc:(Token list -> string) -> Arg<'a>

val cmdError<'a> : Arg<'a>

module Int32 =
    val tryParse: error:string -> input:string -> Result<int, string>
module DateTime =
    val tryParse: error:string -> input:string -> Result<DateTime, string>


val tryParse:  Arg<'a> -> Token list -> Result<'a, string list * Usage list>
val complete: Arg<'a> -> int -> Token list -> string list


val printUsage: Usage list -> unit

/// parse the command line arguments with the specifier arg parser and pass
/// parsed result to the suppied function
val run: appName:string -> arg:Arg<'a> -> cmdLine:string[] -> ('a -> unit) -> int

module Completer =
    /// an empty completer than returns no suggestions 
    val empty: Completer

    // a completer that returns suggestions from specified list
    val choices: choices:string list -> Completer

module Opertators =
    val (<|>): Arg<'a> -> Arg<'a> -> Arg<'a>
    val (|>>): Arg<'a> -> 'b -> Arg<'b>

module Pipe =
    /// reads argument value from standard input
    val stdIn : Arg<string list>

    val orStdIn: Arg<string option> -> Arg<string list>
        
module Env =
    /// Reads argument value from specified environment variable
    val envVar: name:string -> Arg<string option>



        
