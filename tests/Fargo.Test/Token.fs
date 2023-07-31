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
let ``Tokens.ofList should produce correst Start and End``() = 
    Tokens.ofList ["cmd";"--arg";"value"]
    =! [ token "cmd" 0 3  
         token "--arg" 4 9
         token "value" 10 15]

[<Fact>]
let ``Tokens.ofList should skip null entries``() = 
    Tokens.ofList ["cmd";null;"value"]
    =! [ token "cmd" 0 3
         token "value" 4 9 ]
[<Fact>]
let ``Tokens.ofString should accept null``() = 
    Tokens.ofString null
    =! []

[<Fact>]
let ``Tokens.ofString should split spaces``() = 
    Tokens.ofString "cmd --arg value"
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         token "value" 10 15 ]

[<Fact>]
let ``Tokens.ofString should accept multiple spaces and produce Start and End accordingly``() = 
    Tokens.ofString "cmd   --arg  value"
    =! [ token "cmd" 0 3
         token "--arg" 6 11
         token "value" 13 18 ]

[<Fact>]
let ``Tokens.ofString should treat double quotes as a single token and remove quotes``() = 
    Tokens.ofString """cmd --arg "some value" --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '"' "some value" 11 21 
         token "--flag" 23 29 ]

[<Fact>]
let ``Tokens.ofString should not fail on missing end double quote``() = 
    Tokens.ofString """cmd --arg "some value --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         sqtoken '"' "some value --flag" 11 28 ]

[<Fact>]
let ``Tokens.ofString should treat single quotes as a single token and remove quotes``() = 
    Tokens.ofString """cmd --arg 'some value' --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '\'' "some value" 11 21
         token "--flag" 23 29]

[<Fact>]
let ``Tokens.ofString should not fail on missing end single quote``() = 
    Tokens.ofString """cmd --arg 'some value --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         sqtoken '\'' "some value --flag" 11 28 ]

[<Fact>]
let ``Tokens.ofString should accept single quotes in double quotes``() = 
    Tokens.ofString """cmd --arg "some 'value" --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '"' "some 'value" 11 22
         token "--flag" 24 30]

[<Fact>]
let ``Tokens.ofString should accept double quotes in single quotes``() = 
    Tokens.ofString """cmd --arg 'some "value' --flag"""
    =! [ token "cmd" 0 3
         token "--arg" 4 9
         qtoken '\'' """some "value""" 11 22
         token "--flag" 24 30 ]

[<Property>]
let ``Tokens.ofCmdLine should behave as Tokens.ofString for single argument, Tokens.ofList otherwhise`` (args: string[]) =
    match args with
    | [| x |] -> Tokens.ofCmdLine args = Tokens.ofString x
    | _ -> Tokens.ofCmdLine args = Tokens.ofList (Array.toList args)

[<Property>]
let ``Tokens.ofString then Token.toString should give same result`` (NonNull (args: string)) =
    let trimmed = args.TrimEnd()
    let result = trimmed |> Tokens.ofString |> Tokens.toString
    result =! trimmed


[<Fact>]
let ``Tokens.ofString then Token.toString should give same result with empty quotes `` () =
    let trimmed = "\"\""
    let result = trimmed |> Tokens.ofString |> Tokens.toString
    result =! trimmed

