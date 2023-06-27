// The token module contains types and functions to split command line
// int tokens
namespace Fargo

open System


/// The kinds of quotes found around a token
type Quotes =
      /// The token is not quoted 
    | NoQuotes
      // The token has quotes on both ends
    | Quotes of char
      // The token starts with a quote but no end quote has been found
    | StartQuote of char

/// Represents a token position
[<Struct>]
type Extent =
    { Start: int
      End: int }

/// Represents a text token in the command line with its positions 
type Token =
    { Text: string
      Extent: Extent
      Quotes: Quotes }

module Extent =
    let withQuotes quotes extent =
        match quotes with
        | NoQuotes -> extent
        | Quotes _ -> { Start = extent.Start - 1; End = extent.End + 1}
        | StartQuote _ -> { extent with Start =extent.Start - 1}

    let contains pos extent =
        pos >= extent.Start && pos <= extent.End

module Token =
    open Microsoft.FSharp.Core.CompilerServices

    let inline extent s e = { Start = s; End = e}

    let outerExtent token =
        token.Extent |> Extent.withQuotes token.Quotes

    let startQuote token =
        match token.Quotes with
        | Quotes q | StartQuote q -> Some q 
        | NoQuotes -> None

    let endQuote token =
        match token.Quotes with
        | Quotes q -> Some q
        | StartQuote _ | NoQuotes -> None

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
                                     Extent = extent pos input.Length
                                     Quotes = NoQuotes}
                        result.Close()
                    | n -> 
                        let txt = input.Substring(pos, n-pos)
                        if txt <> "" then
                            result.Add { Text = txt
                                         Extent = extent pos n
                                         Quotes = NoQuotes }
                        loop input (n+1) &result
    and loopQuote quote (input: string) (pos: int) (result: ListCollector<_> byref) =
            match input.IndexOf(quote, pos+1) with
            | -1 ->
                result.Add { Text = input.Substring(pos+1)
                             Extent = extent (pos+1) input.Length
                             Quotes = StartQuote quote}
                result.Close()
            | n -> 
                let txt = input.Substring(pos+1, n-pos-1)
                if txt <> "" then
                    result.Add { Text = txt
                                 Extent = extent (pos+1) n
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
                result.Add({Text = token; Extent = extent pos (pos + token.Length); Quotes = quotes} )
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
                let extent = outerExtent token
                let startPad = max (extent.Start - pos) 0
                builder.Append(' ', startPad) |> ignore

                startQuote token
                |> Option.iter (fun q -> builder.Append(q) |> ignore)
                
                builder.Append(token.Text) |> ignore
                
                endQuote token
                |> Option.iter (fun q -> builder.Append(q) |> ignore)
                    

                loop extent.End rest
            | [] -> builder.ToString()
        loop 0 (tokens |> List.sortBy (fun t -> t.Extent))

