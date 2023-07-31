module Fargo.Completion
open Fargo
open Fargo.Operators 

open Xunit
open FsCheck
open FsCheck.Xunit
open DEdge.Diffract

let complete (arg: Arg<_>) pos cmdLine =
    complete arg pos (Tokens.ofString cmdLine)

let (=!) (actual:'a) (expected: 'a) = Differ.Assert(expected, actual )

module Flag =

    [<Fact>]
    let ``complete at the end of cmd line suggest current usage``() =
        let f = flag "flag" "f" "desc"
        complete f 5 "some "
        =! ["--flag"; "-f"]

    [<Fact>]
    let ``complete doesn't extend if token doesn't match start``() =
        let f = flag "flag" "f" "desc"
        complete f 7 "some -x"
        =! []

    [<Fact>]
    let ``complete does extend if strictly after``() =
        let f = flag "flag" "f" "desc"
        complete f 8 "some -x "
        =! [ "--flag"; "-f" ]


    [<Fact>]
    let ``complete do extend if token matches start``() =
        let f = flag "flag" "f" "desc"
        complete f 9 "some --fl"
        =! [ "--flag" ]

    [<Fact>]
    let ``complete do extend if token matches start even in middle of token``() =
        let f = flag "flag" "f" "desc"
        complete f 7 "some --fl"
        =! [ "--flag" ]

    [<Fact>]
    let ``complete doesn't suggest anymore if arg as already been matched``() =
        let f = flag "flag" "f" "desc"
        complete f 13 "some --flag -"
        =! []

module Arg =

    [<Fact>]
    let ``complete at the end of cmd line suggest current usage``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 5 "some "
        =! ["--arg"; "-a"]

    [<Fact>]
    let ``complete at the end of cmd line suggest custom completer if arg matches``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 11 "some --arg "
        =! ["value1"; "value2"]

    [<Fact>]
    let ``complete doesn't extend if token doesn't match start``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 7 "some -x"
        =! []

    [<Fact>]
    let ``complete does extend if strictly after``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 8 "some -x "
        =! [ "--arg"; "-a" ]

    [<Fact>]
    let ``complete does extend if token matches start``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 9 "some --ar"
        =! [ "--arg" ]

    [<Fact>]
    let ``complete does extend if token matches start even in middle of token``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 7 "some --ar"
        =! [ "--arg" ]

    [<Fact>]
    let ``complete doesn't suggest anymore if arg as already been matched``() =
        let f = optc "arg" "a" "value" "desc" (Completer.choices ["value1"; "value2"])
        complete f 19 "some --arg value2 -"
        =! []

module Bind =

    [<Fact>]
    let ``completion before bind should complete first one``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "cmd"
                let! f = flag "flag" "f" "flag"
                return c,f
            }
        complete p 2 "cm"
        =! ["cmd"]

    [<Fact>]
    let ``completion before anything should complete first one``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "cmd"
                let! f = flag "flag" "f" "flag"
                return c,f
            }
        complete p 0 ""
        =! ["cmd"]

    [<Fact>]
    let ``completion after matching first one should suggest second``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "cmd"
                let! f = flag "flag" "f" "flag"
                return c,f
            }
        complete p 4 "cmd "
        =! ["--flag";"-f"]

    [<Fact>]
    let ``completion after failing first one return nothing``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "cmd"
                let! f = flag "flag" "f" "flag"
                return c,f
            }
        complete p 6 "other "
        =! []

    [<Fact>]
    let ``completion after matching both returns nothing ``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "cmd"
                let! f = flag "flag" "f" "flag"
                return c,f
            }
        complete p 11 "cmd --flag "
        =! []

module Applicative =
    [<Fact>]
    let ``completion should suggest all if none is matched``() =
        let p =
            fargo {
                let! a = opt "arg" "a" "value" "arg"
                and! f = flag "flag" "f" "flag"
                return a,f
            }
        complete p 4 "cmd "
        =! ["--arg";"-a";"--flag";"-f"]

    [<Fact>]
    let ``completion before anything should suggest all``() =
        let p =
            fargo {
                let! a = opt "arg" "a" "value" "arg"
                and! f = flag "flag" "f" "flag"
                return a,f
            }
        complete p 0 ""
        =! ["--arg";"-a";"--flag";"-f"]

    [<Fact>]
    let ``completion after matching first one should suggest second``() =
        let p =
            fargo {
                let! a = opt "arg" "a" "value" "arg"
                and! f = flag "flag" "f" "flag"
                return a,f
            }
        complete p 13 "cmd -a value "
        =! ["--flag";"-f"]

    [<Fact>]
    let ``completion after matching second should suggest first one``() =
        let p =
            fargo {
                let! a = opt "arg" "a" "value" "arg"
                and! f = flag "flag" "f" "flag"
                return a,f
            }
        complete p 14 "cmd -x --flag "
        =! [ "--arg"; "-a" ]

    [<Fact>]
    let ``completion after matching both returns nothing ``() =
        let p =
            fargo {
                let! a = opt "arg" "a" "value" "arg"
                and! f = flag "flag" "f" "flag"
                return a,f
            }
        complete p 18 "cmd -a 123 --flag "
        =! []


module Alt =
    [<Fact>]
    let ``completion should suggest all if none is matched``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "cmd2" "c2" "desc cmd2"
        complete p 1 "c"
        =! ["cmd1";"c1";"cmd2";"c2"]

    [<Fact>]
    let ``completion should suggest only ones matching prefix``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "o" "desc other"
        complete p 1 "c"
        =! ["cmd1";"c1"]


    [<Fact>]
    let ``completion should suggest only ones matching prefix (second)``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "ot" "desc other"
        complete p 1 "o"
        =! ["other";"ot"]

    [<Fact>]
    let ``completion should suggest nothing when prefix doesnt match``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "ot" "desc other"
        complete p 1 "x"
        =! []


    [<Fact>]
    let ``completion should suggest nothing if one of alt has been matched``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "ot" "desc other"
        complete p 5 "cmd1 "
        =! []

    [<Fact>]
    let ``completion should suggest nothing if one of alt has been matched (second)``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "ot" "desc other"
        complete p 6 "other "
        =! []

    [<Fact>]
    let ``completion should suggest nothing if none matched``() =
        let p =
            cmd "cmd1" "c1" "desc cmd1"
            <|> cmd "other" "ot" "desc other"
        complete p 10 "something "
        =! []
