module Fargo.AllAtOnce

open Fargo
open Fargo.Opertators
open Xunit

type Voice = Soft | Standard | Loud | Funny

type MainCmd =
    | Say
    | Voice

type VoiceCmd =
    | List
    | Select

type Args=
    | Say of text:string * Voice option * volume: int option * reverse: bool
    | ListVoice
    | SelectVoice of Voice * volume: int option

let parseVoice = function
    | "soft" -> Ok Soft
    | "standard" -> Ok Standard
    | "loud" -> Ok Loud
    | "funny" -> Ok Funny
    | v -> Error $"Unknown voice {v}"

let voiceCompleter =
    Completer.choices [ "soft"; "standard"; "loud"; "funny" ]

let pVolume = arg "volume" "vl" "the volume of the voice" |> optParse (Int32.tryParse "Invalid volume") 
let pVoice = arg "voice" "vc" "the voice to use" |> completer voiceCompleter |> optParse parseVoice
let p =
    cmdLine {
        match!
            (cmd "say" null "Says specified text" |>> MainCmd.Say)
            <|> (cmd "voice" "vo" "Voice  commands" |>> MainCmd.Voice)
            <|> cmdError
            with
        | MainCmd.Say ->
            let! text = arg "text" "t" "the text to say" |> reqArg
            and! voice = pVoice
            and! volume = pVolume 
            and! reverse = flag "reverse" "r" "say words in reverse order"
            return Say(text, voice, volume, reverse)
        | MainCmd.Voice ->
            match! (cmd "list" "ls" "List voices" |>> VoiceCmd.List)
                   <|>(cmd "select" "sel" "Select default voice" |>> VoiceCmd.Select)
                   <|> cmdError with
            | VoiceCmd.List ->
                return ListVoice
            | VoiceCmd.Select ->
                let! voice = pVoice |> reqArg
                and! volume = pVolume
                return SelectVoice(voice, volume)
    }

let (=!) (actual:'a) (expected:'a) = Assert.Equal<'a>(expected, actual)


let parse input =
    tryParse p (Token.ofString input)
    |> Result.mapError (fun (errs,usages) -> errs, [ for u in usages -> u.Name ])

let complete pos input =
    complete p pos (Token.ofString input)


[<Fact>]
let ``Empty input returns usage``() =
    parse ""
    =! Error(["Missing command"], [ "say"; "voice"] )

[<Fact>]
let ``Unknown command input returns usage``() =
    parse "other"
    =! Error(["Unknown command other"], [ "say"; "voice"] )


[<Fact>]
let ``missing required text arg``() =
    parse "say "
    =! Error(["Required argument --text not found"], [ "--text"; "--voice"; "--volume"; "--reverse"] )

[<Fact>]
let ``Successful say command``() =
    parse """say --text "hello world" """
    =! Ok (Say("hello world", None, None, false))

[<Fact>]
let ``Successful say command with voice``() =
    parse """say --voice funny --text "hello world" """
    =! Ok (Say("hello world", Some Funny, None, false))

[<Fact>]
let ``Successful say command with unknown voice``() =
    parse """say --voice sleepy --text "hello world" """
    =! Error(["Unknown voice sleepy"], [ "--text"; "--voice"; "--volume"; "--reverse"] )

[<Fact>]
let ``Successful say command with volume``() =
    parse """say --voice funny --text "hello world" -vl 10 """
    =! Ok (Say("hello world", Some Funny, Some 10, false))

[<Fact>]
let ``Successful say command with unknown volume``() =
    parse """say --voice loud --text "hello world" -vl none """
    =! Error(["Invalid volume"], [ "--text"; "--voice"; "--volume"; "--reverse"] )

[<Fact>]
let ``Successful say command with reverse flag``() =
    parse """say --voice funny --text "hello world" --reverse -vl 10 """
    =! Ok (Say("hello world", Some Funny, Some 10, true))




[<Fact>]
let ``Missing sub command``() =
    parse "voice"
    =! Error(["Missing command"], [ "list"; "select"] )


[<Fact>]
let ``Invalid sub command``() =
    parse "voice raise"
    =! Error(["Unknown command raise"], [ "list"; "select"] )

[<Fact>]
let ``Valid sub command``() =
    parse "voice list"
    =! Ok ListVoice

[<Fact>]
let ``Valid sub command without required arg``() =
    parse "voice select"
    =! Error (["Required argument --voice not found" ], ["--voice"; "--volume"])

[<Fact>]
let ``Valid sub command with required arg``() =
    parse "voice select --voice loud"
    =! Ok (SelectVoice(Loud, None))

[<Fact>]
let ``Valid sub command with required arg and optional one``() =
    parse "voice select --voice loud --volume 5"
    =! Ok (SelectVoice(Loud, Some 5))

[<Fact>]
let ``Valid sub command with required arg and invalid optional one``() =
    parse "voice select --voice loud --volume mute"
    =! Error (["Invalid volume" ], ["--voice"; "--volume"])

[<Fact>]
let ``Valid sub command with required arg but missing value ``() =
    parse "voice select --voice "
    =! Error (["Argument --voice value is missing" ], ["--voice"; "--volume"])

[<Fact>]
let ``--help return local usage even when command matches``() =
    parse "voice select --voice funny --help"
    =! Error([], ["--voice";"--volume"])

[<Fact>]
let ``--help return local usage even when command fails``() =
    parse "voice select --voice nope --help"
    =! Error([], ["--voice";"--volume"])

[<Fact>]
let ``--help return local usage for partial parsing``() =
    parse "voice --help"
    =! Error([], ["list";"select"])

[<Fact>]
let ``--help return main usage``() =
    parse "--help"
    =! Error([], ["say";"voice"])

[<Fact>]
let ``completion on - returns all args``() =
    complete 14 "voice select -"
    =! [ "--voice"; "-vc"; "--volume"; "-vl"]

[<Fact>]
let ``completion after command returns all args``() =
    complete 13 "voice select "
    =! [ "--voice"; "-vc"; "--volume"; "-vl"]

[<Fact>]
let ``completion after voice arg returns voice names``() =
    complete 21 "voice select --voice "
    =! ["soft"; "standard"; "loud"; "funny"]








