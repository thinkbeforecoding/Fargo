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


    val req: Usages -> Usages

    module Short =
        val ofString : string -> string option




[<AutoOpen>]
module Fargo =
    /// A command parser. Commands have specified name or alt, and are always matched as the first token
    val cmd: name:string -> alt:string -> description:string -> Arg<string>

    /// An option parser with a completer. Options are matched by name and are optional by default.
    val optc: name:string -> alt:string -> value: string -> description:string -> completer: Completer -> Arg<string option>
    /// An option parser. Options are specified by name and are optional by default.
    val opt: name:string -> alt:string -> value:string -> description:string -> Arg<string option>

    /// Make an option required
    val reqOpt: arg:Arg<'a option> -> Arg<'a> 

    /// An argument parser with a completer. Arguments is position based and is optional by default
    val argc: value: string -> description:string -> completer: Completer -> Arg<string option>

    /// An argument parser. Arguments is position based and is optional by default
    val arg: value: string -> description:string -> Arg<string option>

    /// Make an argument required
    val reqArg: Arg<'a option> -> Arg<'a>

    /// A flag parser. Flags are matched by name and are optional by default. Value is false when not specified
    val flag: string -> string -> string -> Arg<bool>

    /// Make a flag required.
    val reqFlag: Arg<bool> -> Arg<bool>

    /// Parse an arg value. If value parsing fails, the argument parsing stops with specified error message.
    val parse: ('a -> Result<'b, string>) ->  Arg<'a> -> Arg<'b>

    /// Parse an arg optional value. If value parsing fails, the argument parsing stops with specified error message.
    val optParse: ('a -> Result<'b, string>) -> Arg<'a option> -> Arg<'b option>

    /// Parse all elements of a list argument. The parsing fails if any value parsing fails.
    val listParse: ('a -> Result<'b, string>) -> Arg<'a list> -> Arg<'b list>

    /// An argument containing all the remaining tokens.
    val all: value:string -> description: string -> Arg<Tokens>


    /// Validates argument value. If validation fails, error message is used.
    val validate: f:('a -> bool) -> error:string -> arg:Arg<'a> -> Arg<'a>

    /// Validates argument value. If validation fails, error message is used.
    val optValidate: f:('a -> bool) -> error:string -> arg: Arg<'a option> -> Arg<'a option>

    /// Validates that the argument list value is not empty.
    val nonEmpty: string -> Arg<'a list> -> Arg<'a list>

    /// Converts an argument value using specified function
    val map: ('a -> 'b) -> Arg<'a> -> Arg<'b>
    /// Converts an optional argument value using specified function
    val optMap: ('a -> 'b) -> Arg<'a option> -> Arg<'b option>

    /// Use specified value for optional argument when argument is missing.
    val defaultValue: 'a ->  Arg<'a option>  -> Arg<'a>

    /// Combines two arguments values using specified function.
    val map2: ('a -> 'b -> 'c) -> Arg<'a> -> Arg<'b> -> Arg<'c>

    /// Returns new parser depending on an argument (usually a cmd) value.
    val bind: ('a -> Arg<'b>) -> Arg<'a> -> Arg<'b>

    /// Creates an arguments that has the given value.
    val ret: 'a -> Arg<'a>

    /// The builder for the fargo Computation Expression.
    [<Class>]
    type FargoBuilder =
        member Bind: Arg<'a> * ('a -> Arg<'b>) -> Arg<'b>
        member BindReturn: Arg<'a> * ('a -> 'b) -> Arg<'b>
        member MergeSources:  Arg<'a> * Arg<'b> -> Arg<'a * 'b>
        member Return: 'a -> Arg<'a>
        member ReturnFrom: Arg<'a> -> Arg<'a>
        member Zero: unit -> Arg<unit>

    /// The fargo Computation Expression.
    val fargo: FargoBuilder

    /// Returns x value if parsing succeeds, otherwhite return y value.
    val alt: y:Arg<'a> -> x:Arg<'a> -> Arg<'a>
    /// Returns x value if parsing succeeds and has a value, otherwhite return y value.
    val optAlt: Arg<'a option> -> Arg<'a option> -> Arg<'a option>

    /// Creates an argument that always returns given error message
    val error: message:string -> Arg<'a>

    /// Creates an argument that always returns given error message
    val errors: messages:string list -> Arg<'a>



    /// Creates an argument that returns a error message depending on remaining tokens.
    val errorf<'a> : messageFunc:(Token list -> string) -> Arg<'a>

    /// Creates an arg that returns a error message indicating an unknown cmd.
    val cmdError<'a> : Arg<'a>

    /// Add specific help message to specified argument.
    val help: text:string -> arg: Arg<'t> -> Arg<'t> 

module Operators =
    /// Take value on the left, or the value on the right.
    val (<|>) : Arg<'a> -> Arg<'a> -> Arg<'a>
    /// Take value on the left, or the value on the right.
    val (<|?>) : Arg<'a option> -> Arg<'a option> -> Arg<'a option>
    /// Change the value of an argument with the specified constant.
    val (|>>) : Arg<'a> -> 'b -> Arg<'b>


[<AutoOpen>]
module Run =
    val toResult: arg: Arg<'a> -> Arg<ParseResult<'a> * Usages>

    val tryParseTokens:  arg:Arg<'a> -> tokens:Tokens -> Result<'a, string list * Usages>
    val complete: Arg<'a> -> int -> Tokens -> string list


    val printErrors: string seq -> unit
    val printUsage: Usages -> unit
    val printDescription: Usages -> unit
    val printOptions: Usage list -> unit
    val printHelp: Usages -> unit

    type Shell = Powershell | Fish | Bash

    val printCompletion: appName:string -> shell:Shell -> unit

    val getUsages : arg:Arg<'a> -> Arg<Result<'a, string list> * Usages>

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



        
