module Token
open Xunit
open FsCheck
open FsCheck.Xunit
open DEdge.Diffract

open Fargo




let (=!) (actual:'a) (expected: 'a) = Differ.Assert(expected, actual )


let token text s e = { Text = text; Extent = {Start = s; End = e }; Quotes = NoQuotes} 
let qtoken q text s e = { Text = text; Extent = {  Start = s; End = e}; Quotes = Quotes q} 
let sqtoken q text s e = { Text = text; Extent = { Start = s; End = e}; Quotes = StartQuote q} 

[<Fact>]
let ``Token.ofList should produce correst Start and End``() = 
    Token.ofList ["cmd";"--arg";"value"]
    =! [ token "cmd" 0 3  
         token "--arg" 4 9
         token "value" 10 15]

[<Fact>]
let ``Token.ofList should skip null entries``() = 
    Token.ofList ["cmd";null;"value"]
    =! [ token "cmd" 0 3
         token "value" 4 9 ]
[<Fact>]
let ``Token.ofString should accept null``() = 
    Token.ofString null
    =! []

[<Fact>]
let ``Token.ofString should split spaces``() = 
    Token.ofString "cmd --arg value"
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         token "value" 10 15 ]

[<Fact>]
let ``Token.ofString should accept multiple spaces and produce Start and End accordingly``() = 
    Token.ofString "cmd   --arg  value"
    =! [ token "cmd" 0 3
         token "--arg" 6 11
         token "value" 13 18 ]

[<Fact>]
let ``Token.ofString should treat double quotes as a single token and remove quotes``() = 
    Token.ofString """cmd --arg "some value" --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '"' "some value" 11 21 
         token "--flag" 23 29 ]

[<Fact>]
let ``Token.ofString should not fail on missing end double quote``() = 
    Token.ofString """cmd --arg "some value --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         sqtoken '"' "some value --flag" 11 28 ]

[<Fact>]
let ``Token.ofString should treat single quotes as a single token and remove quotes``() = 
    Token.ofString """cmd --arg 'some value' --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '\'' "some value" 11 21
         token "--flag" 23 29]

[<Fact>]
let ``Token.ofString should not fail on missing end single quote``() = 
    Token.ofString """cmd --arg 'some value --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         sqtoken '\'' "some value --flag" 11 28 ]

[<Fact>]
let ``Token.ofString should accept single quotes in double quotes``() = 
    Token.ofString """cmd --arg "some 'value" --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '"' "some 'value" 11 22
         token "--flag" 24 30]

[<Fact>]
let ``Token.ofString should accept double quotes in single quotes``() = 
    Token.ofString """cmd --arg 'some "value' --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '\'' """some "value""" 11 22
         token "--flag" 24 30 ]

[<Property>]
let ``Token.ofCmdLine should behave as Token.ofString for single argument, Token.ofList otherwhise`` (args: string[]) =
    match args with
    | [| x |] -> Token.ofCmdLine args = Token.ofString x
    | _ -> Token.ofCmdLine args = Token.ofList (Array.toList args)

[<Property>]
let ``Token.ofString then Token.toString should give same result`` (NonNull (args: string)) =
    let trimmed = args.TrimEnd()
    let result = trimmed |> Token.ofString |> Token.toString
    result =! trimmed


[<Fact>]
let ``Token.ofString then Token.toString should give same result with empty quotes `` () =
    let trimmed = "\"\""
    let result = trimmed |> Token.ofString |> Token.toString
    result =! trimmed

