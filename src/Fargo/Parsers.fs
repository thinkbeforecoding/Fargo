module Fargo.Parsers
open System

let error message value =
    match value with
    | Some v -> Ok v
    | None -> Error message

module Int32 =
    let tryParse (input: string) =
        match Int32.TryParse(input, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | false, _ -> None

module DateTime =
    let tryParse (input: string) =
        match DateTime.TryParse(input, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AssumeUniversal) with
        | true, v -> Some v
        | false, _ -> None
