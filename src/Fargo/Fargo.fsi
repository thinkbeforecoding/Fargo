namespace Fargo

open System

[<AutoOpen>]
module Core =
    type Completer = string -> Token list -> string list

    [<Flags>]
    type UsageType =
    | None = 0
    | Arg = 1
    | Required = 2
    | Many = 4

    type Usage = { Name: string option; Alt: string option; Value: string option; Description: string; Help: string option; Type: UsageType}
        with
            member IsRequired : bool 
            member IsArg: bool 
            member IsMany: bool

    type Usages =
        { Path:  Usage list
          Options: Usage list }
    type Tokens = Token list

    type ParseResult<'t> = Result<'t, string list>

    type Parse<'a> = Tokens -> ParseResult<'a> * Tokens * Usages
    type Complete = int -> Tokens -> string list * bool
    type Arg<'a> =
        { Parse: Parse<'a>
          Complete: Complete }

  module Usages =
    val merge : Usages -> Usages -> Usages

    val empty: Usages

module Usage =
    val isMatch: Usage -> Token -> bool
    val isStop: int -> Token -> bool
    
    val (|IsPrefix|_|): int -> Token -> unit option

    val complete: Usage -> string -> string list * bool




[<AutoOpen>]
module Fargo =
    /// parse the command line arguments with the specifier arg parser and pass
    /// parsed result to the suppied function
    val cmd: name:string -> alt:string -> description:string -> Arg<string>
    val optc: name:string -> alt:string -> value: string -> description:string -> completer: Completer -> Arg<string option>
    val opt: name:string -> alt:string -> value:string -> description:string -> Arg<string option>

    val reqOpt: arg:Arg<'a option> -> Arg<'a> 

    val argc: value: string -> description:string -> completer: Completer -> Arg<string option>
    val arg: value: string -> description:string -> Arg<string option>

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
    type FargoBuilder =
        member Bind: Arg<'a> * ('a -> Arg<'b>) -> Arg<'b>
        member BindReturn: Arg<'a> * ('a -> 'b) -> Arg<'b>
        member MergeSources:  Arg<'a> * Arg<'b> -> Arg<'a * 'b>
        member Return: 'a -> Arg<'a>
        member ReturnFrom: Arg<'a> -> Arg<'a>
        member Zero: unit -> Arg<unit>

    val fargo: FargoBuilder

    val alt: Arg<'a> -> Arg<'a> -> Arg<'a>
    val optAlt: Arg<'a option> -> Arg<'a option> -> Arg<'a option>
    val error: message:string -> Arg<'a>
    val errorf<'a> : messageFunc:(Token list -> string) -> Arg<'a>

    val cmdError<'a> : Arg<'a>

    val help: text:string -> arg: Arg<'t> -> Arg<'t> 

module Operators =
    val (<|>) : Arg<'a> -> Arg<'a> -> Arg<'a>
    val (<|?>) : Arg<'a option> -> Arg<'a option> -> Arg<'a option>
    val (|>>) : Arg<'a> -> 'b -> Arg<'b>


[<AutoOpen>]
module Run =
    val tryParseTokens:  arg:Arg<'a> -> tokens:Tokens -> Result<'a, string list * Usages>
    val complete: Arg<'a> -> int -> Tokens -> string list


    val printUsage: Usages -> unit
    val printHelp: Usages -> unit

    /// parse the command line arguments with the specifier arg parser and pass
    /// parsed result to the suppied function
    val run: appName:string -> arg:Arg<'a> -> cmdLine:string[] -> f:(Threading.CancellationToken -> 'a -> Threading.Tasks.Task<int>) -> int

module Completer =
    /// an empty completer than returns no suggestions 
    val empty: Completer

    // a completer that returns suggestions from specified list
    val choices: choices:string list -> Completer

module Pipe =
    /// reads argument value from standard input
    val stdIn : Arg<string list>

    val orStdIn: Arg<string option> -> Arg<string list>
        
module Env =
    /// Reads argument value from specified environment variable
    val envVar: name:string -> Arg<string option>



        
