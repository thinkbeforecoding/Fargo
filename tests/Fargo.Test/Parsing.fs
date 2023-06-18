module Fargo.Parsing
open Fargo
open Fargo.Opertators 


open Xunit
open FsCheck
open FsCheck.Xunit

let (=!) (actual:'a) (expected:'a) = Assert.Equal<'a>(expected, actual)


let parse (arg: Arg<'t>) input =
    let result, _, _ = arg ValueNone (Token.ofString input)
    result

let rest (arg: Arg<'t>) input =
    let _, tokens, _ = arg ValueNone (Token.ofString input)
    Token.toString tokens

let usage (arg: Arg<'t>) input =
    let _, _, usages = arg ValueNone (Token.ofString input)
    usages
    


[<Fact>]
let ``flag name parsing succeeds``() =
    let f = flag "flag" "f" "description" 
    parse f "cmd --other-arg value --flag -x"
    =! Success true



module Flag =
    [<Fact>]
    let ``flag short name parsing succeeds``() =
        let f = flag "flag" "f" "description" 
        parse f "cmd --other-arg value -f -x"
        =! Success true

    [<Fact>]
    let ``Absence of flag succeeds with value false ``() =
        let f = flag "flag" "f" "description" 
        parse f "cmd --other-arg value -x"
        =! Success false

    [<Fact>]
    let ``flag token is removed after matching``() =
        let f = flag "flag" "f" "description" 
        rest f "cmd --other-arg value --flag -x"
        =!  "cmd --other-arg value        -x"

    [<Fact>]
    let ``flag not matching keep tokens intact``() =
        let f = flag "flag" "f" "description" 
        rest f "cmd --other-arg value --fl -x"
        =! "cmd --other-arg value --fl -x"

    [<Fact>]
    let ``flag usage is returned``() =
        let f = flag "flag" "f" "description" 
        usage f "cmd --other-arg value --flag -x"
        =! [ { Name = "--flag"; Alt = Some "-f"; Description = "description" } ]

module ReqFlag =
    [<Fact>]
    let ``reqFlag name parsing succeeds``() =
        let f = flag "flag" "f" "description" |> reqFlag
        parse f "cmd --other-arg value --flag -x"
        =! Success true

    [<Fact>]
    let ``reqFlag short name parsing succeeds``() =
        let f = flag "flag" "f" "description"  |> reqFlag
        parse f "cmd --other-arg value -f -x"
        =! Success true

    [<Fact>]
    let ``reqFlag token is removed after matching``() =
        let f = flag "flag" "f" "description" |> reqFlag
        rest f "cmd --other-arg value --flag -x"
        =! "cmd --other-arg value        -x"

    [<Fact>]
    let ``reqFlag not matching keep tokens intact``() =
        let f = flag "flag" "f" "description" |> reqFlag
        rest f "cmd --other-arg value --fl -x"
        =! "cmd --other-arg value --fl -x"

    [<Fact>]
    let ``Absence of reqFlag fails``() =
        let f = flag "flag" "f" "description" |> reqFlag
        parse f "cmd --other-arg value -x"
        =! Failure ["Required flag --flag not found"]

    [<Fact>]
    let ``reqFlag usage is returned if it succeeds``() =
        let f = flag "flag" "f" "description" |> reqFlag
        usage f "cmd --other-arg value --flag -x"
        =! [ { Name = "--flag"; Alt = Some "-f"; Description = "description (required)" } ]

    [<Fact>]
    let ``reqFlag usage is returned if it fails``() =
        let f = flag "flag" "f" "description" |> reqFlag
        usage f "cmd --other-arg value -x"
        =! [ { Name = "--flag"; Alt = Some "-f"; Description = "description (required)" } ]

module Cmd =
    [<Fact>]
    let ``cmd name parsing succeeds when in first position``() =
        let c = cmd "command" "cmd" "description" 
        parse c "command --other-arg value --flag -x"
        =! Success "command"

    [<Fact>]
    let ``cmd short name parsing succeeds when in first position``() =
        let c = cmd "command" "cmd" "description" 
        parse c "cmd --other-arg value -f -x"
        =! Success "command"
    
    [<Fact>]
    let ``cmd short name parsing fails when not in first position``() =
        let c = cmd "command" "cmd" "description" 
        parse c "something command --other-arg value -f -x"
        =! Failure [ "Command command not found"]
        
    [<Fact>]
    let ``cmd token is removed after matching``() =
        let c = cmd "command" "cmd" "description" 
        rest c "command --other-arg value --flag -x"
        =!  "        --other-arg value --flag -x"

    [<Fact>]
    let ``cmd not matching keep tokens intact``() =
        let c = cmd "command" "cmd" "description" 
        rest c "something --other-arg value --flag -x"
        =!  "something --other-arg value --flag -x"

    [<Fact>]
    let ``Absence of cmd fails``() =
        let c = cmd "command" "cmd" "description" 
        parse c "somthing --other-arg value -x"
        =! Failure ["Command command not found"]

    [<Fact>]
    let ``cmd usage is returned if it succeeds``() =
        let c = cmd "command" "cmd" "description" 
        usage c "cmd --other-arg value --flag -x"
        =! [ { Name = "command"; Alt = Some "cmd"; Description = "description" } ]

    [<Fact>]
    let ``cmd usage is returned if it failse``() =
        let c = cmd "command" "cmd" "description" 
        usage c "something --other-arg value --flag -x"
        =! [ { Name = "command"; Alt = Some "cmd"; Description = "description" } ]

module Arg =
    [<Fact>]
    let ``arg name parsing succeeds``() =
        let f = arg "arg" "a" "description"
        parse f "cmd --other-arg val --arg value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg short name parsing succeeds``() =
        let f =  arg "arg" "a" "description"
        parse f "cmd --other-arg val -a value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg and its value tokens are removed after matching``() =
        let f =  arg "arg" "a" "description"
        rest f "cmd --other-arg val --arg value -x"
        =!  "cmd --other-arg val             -x"

    [<Fact>]
    let ``arg token is removed after if matching only name``() =
        let f =  arg "arg" "a" "description"
        rest f "cmd --other-arg val -x --arg"
        =!  "cmd --other-arg val -x"

    [<Fact>]
    let ``arg doesn't change token if not matching``() =
        let f = arg "arg" "a" "description"
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of arg succeeds with None``() =
        let f =  arg "arg" "a" "description"
        parse f "cmd --other-arg value -x"
        =! Success None

    [<Fact>]
    let ``arg without it's value fails``() =
        let f =  arg "arg" "a" "description"
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]


    [<Fact>]
    let ``arg usage is returned if it succeeds``() =
        let f =  arg "arg" "a" "description"
        usage f "cmd --other-arg val --arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" } ]

    [<Fact>]
    let ``arg usage is returned if absent``() =
        let f =  arg "arg" "a" "description"
        usage f "cmd --other-arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" } ]
    
    [<Fact>]
    let ``arg usage is returned if only name``() =
        let f =  arg "arg" "a" "description"
        usage f "cmd --other-arg value -x --arg"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" } ]

module ReqArg =
    [<Fact>]
    let ``req name parsing succeeds``() =
        let f = arg "arg" "a" "description" |> reqArg
        parse f "cmd --other-arg val --arg value -x"
        =! Success ("value")

    [<Fact>]
    let ``req short name parsing succeeds``() =
        let f =  arg "arg" "a" "description" |> reqArg
        parse f "cmd --other-arg val -a value -x"
        =! Success ("value")

    [<Fact>]
    let ``req and its value tokens are removed after matching``() =
        let f =  arg "arg" "a" "description" |> reqArg
        rest f "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"
    
    [<Fact>]
    let ``req token is removed if matching only name``() =
        let f = arg "arg" "a" "description" |> reqArg
        rest f "cmd --other-arg val -x --arg"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``req doesn't change token if not matching``() =
        let f = arg "arg" "a" "description" |> reqArg
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of req fails``() =
        let f =  arg "arg" "a" "description" |> reqArg
        parse f "cmd --other-arg value -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``req without it's value fails``() =
        let f =  arg "arg" "a" "description" |> reqArg
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]

    [<Fact>]
    let ``req usage is returned if it succeeds``() =
        let f =  arg "arg" "a" "description" |> reqArg
        usage f "cmd --other-arg val --arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description (required)" } ]

    [<Fact>]
    let ``req usage is returned if absent``() =
        let f =  arg "arg" "a" "description" |> reqArg
        usage f "cmd --other-arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description (required)" } ]
    
    [<Fact>]
    let ``req usage is returned if only name``() =
        let f =  arg "arg" "a" "description" |> reqArg
        usage f "cmd --other-arg value -x --arg"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description (required)" } ]


module Applicative =
    [<Fact>]
    let ``Applicative can succeed``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -f -x"
        =! Success( Some "value", true)

    [<Fact>]
    let ``Applicative combine results``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Success( Some "value", false)

    [<Fact>]
    let ``Applicative combine results (other direction)``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val -f -x"
        =! Success( None, true)

    [<Fact>]
    let ``Applicative remove tokens when it succeeds``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd -f --other-arg val --arg value -x"
        =! "cmd    --other-arg val             -x"

    [<Fact>]
    let ``Applicative fails if first fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg"  |> reqArg
                and! f = flag "flag" "f" "description flag"  |> reqFlag
                return a,f
            }

        parse p "cmd -f --other-arg val -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``Applicative fails if second fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg"  |> reqArg
                and! f = flag "flag" "f" "description flag" |> reqFlag
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Failure ["Required flag --flag not found"]

    [<Fact>]
    let ``Applicative combines failures if both fail``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg"  |> reqArg
                and! f = flag "flag" "f" "description flag" |> reqFlag
                return a,f
            }

        parse p "cmd --other-arg val -x"
        =! Failure [ "Required argument --arg not found"
                     "Required flag --flag not found"]

    [<Fact>]
    let ``Applicative remove matched tokens when first fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -f -x"
        =! "cmd --other-arg val    -x"

    [<Fact>]
    let ``Applicative remove matched tokens when second fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"

    [<Fact>]
    let ``Applicative leaves token intact when nothing matches``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"


    [<Fact>]
    let ``Applicative combine usages when it succeeds``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val --arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description arg" }
             { Name = "--flag"; Alt = Some "-f"; Description = "description flag" }]

    [<Fact>]
    let ``Applicative combine usages when first fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description arg" }
             { Name = "--flag"; Alt = Some "-f"; Description = "description flag" }]

    [<Fact>]
    let ``Applicative combine usages when second fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val --arg value -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description arg" }
             { Name = "--flag"; Alt = Some "-f"; Description = "description flag" }] 
    
    [<Fact>]
    let ``Applicative combine usages when both fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description arg" }
             { Name = "--flag"; Alt = Some "-f"; Description = "description flag" }] 

module Map =
    [<Property>]
    let ``map changes value when it succeeds`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> map int 
        parse p $"cmd -f --arg %d{value} -x"
        =! Success value

    [<Fact>]
    let ``map keeps failures`` () =
        let p = arg "arg" "a" "description" |> reqArg |> map int
        parse p $"cmd -f -x"
        =! Failure [ "Required argument --arg not found" ]

    [<Property>]
    let ``map keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> map int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``map keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> map int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description (required)" }]

module OptMap =
    [<Property>]
    let ``optMap changes value when it succeeds`` (value: int) =
        let p = arg "arg" "a" "description" |> optMap int
        parse p $"cmd -f --arg %d{value} -x"
        =! Success (Some value)
    [<Fact>]
    let ``optMap return None when it doesn't match``() =
        let p = arg "arg" "a" "description" |> optMap int
        parse p $"cmd -f -x"
        =! Success None
    [<Fact>]
    let ``optMap keeps failures`` () =
        let p = arg "arg" "a" "description" |> optMap int
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``optMap keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" |> optMap int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``optMap keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" |> optMap int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" }]

module Parse =
    let parseInt (input: string) = 
        match System.Int32.TryParse(input, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error "Bad int!"
    [<Property>]
    let ``parse changes value when it succeeds`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> Fargo.parse parseInt
        parse p $"cmd -f --arg %d{value} -x"
        =! Success ( value)

    [<Property>]
    let ``parse fail when value cannot be parsed`` () =
        let p = arg "arg" "a" "description" |> reqArg |> Fargo.parse parseInt
        parse p $"cmd -f --arg value -x"
        =! Failure ["Bad int!"] 

    [<Fact>]
    let ``parse keeps failures`` () =
        let p = arg "arg" "a" "description" |> reqArg |> Fargo.parse parseInt
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``parse keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> Fargo.parse parseInt
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``parse don't consume token when parsing fails`` () =
        let p = arg "arg" "a" "description" Completer.empty |> reqArg |>Fargo.parse parseInt
        rest p $"cmd -f --arg value -x"
        =! "cmd -f --arg value -x"

    [<Property>]
    let ``parse keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" |> reqArg |> Fargo.parse parseInt
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description (required)" }]


module OptParse =
    let parseInt (input: string) = 
        match System.Int32.TryParse(input, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error "Bad int!"
    [<Property>]
    let ``optParse changes value when it succeeds`` (value: int) =
        let p = arg "arg" "a" "description" |> optParse parseInt
        parse p $"cmd -f --arg %d{value} -x"
        =! Success (Some value)

    [<Fact>]
    let ``optParse fail when value cannot be parsed`` () =
        let p = arg "arg" "a" "description" |> optParse parseInt
        parse p $"cmd -f --arg value -x"
        =! Failure ["Bad int!"] 

    [<Fact>]
    let ``optParse return None when it doesn't match``() =
        let p = arg "arg" "a" "description" |> optParse parseInt
        parse p $"cmd -f -x"
        =! Success None
    [<Fact>]
    let ``optParse keeps failures`` () =
        let p = arg "arg" "a" "description" |> optParse parseInt
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``optParse keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" |> optParse parseInt
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Fact>]
    let ``optParse don't consume token when parsing fails`` () =
        let p = arg "arg" "a" "description" |> optParse parseInt
        rest p $"cmd -f --arg value -x"
        =! "cmd -f --arg value -x"

    [<Property>]
    let ``optParse keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" |> optParse parseInt
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" }]


module DefaultValue =

    [<Property>]
    let ``defaultValue keep original value when it succeeds`` (value: int) (def: int) =
        let p = arg "arg" "a" "description" |> defaultValue $"%d{def}"
        parse p $"cmd -f --arg %d{value} -x"
        =! Success $"%d{value}"

    [<Property>]
    let ``defaultValue fail when value cannot be parsed`` (value: int) (def: int) =
        let p = arg "arg" "a" "description" |> defaultValue $"%d{def}"
        parse p $"cmd -f -x"
        =! Success $"%d{def}"

    [<Fact>]
    let ``defaultValue keeps failures`` () =
        let p = arg "arg" "a" "description" |> defaultValue "default"
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``defaultValue keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" |> defaultValue "default"
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``defaultValue don't consume token when parsing fails`` () =
        let p = arg "arg" "a" "description" |> defaultValue "default"
        rest p $"cmd -f -x -a"
        =! "cmd -f -x -a"

    [<Property>]
    let ``defaultValue keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" |> defaultValue "default"
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = "--arg"; Alt = Some "-a"; Description = "description" }]


module Bind =
    
    [<Fact>]
    let ``bind succeeds``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "cmd sub --arg x"
        =! Success("cmd", "sub")

    [<Fact>]
    let ``bind fails if first fails``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "other sub --arg x"
        =! Failure [ "Command cmd not found"]

    [<Fact>]
    let ``bind fails if second fails``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "cmd other --arg x"
        =! Failure [ "Command sub not found"]

    [<Fact>]
    let ``bind consume both tokens``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "cmd sub --arg x"
        =! "        --arg x"

    [<Fact>]
    let ``bind failing on first consumes no token``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "other sub --arg x"
        =! "other sub --arg x"

    [<Fact>]
    let ``bind failing on second consumes first token``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "cmd other --arg x"
        =! "    other --arg x"

    [<Fact>]
    let ``bind success returns last usage``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "cmd sub --arg x"
        =! [ { Name = "sub"; Alt = None; Description = "sub desc"} ]

    [<Fact>]
    let ``bind failing on first returns first  usage``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "other sub --arg x"
        =! [ { Name = "cmd"; Alt = None; Description = "desc"} ]

    [<Fact>]
    let ``bind failing on second returns second usage``() =
        let p =
            cmdLine {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "cmd other --arg x"
        =! [ { Name = "sub"; Alt = None; Description = "sub desc"} ]

module Ret =
    [<Property>]
    let ``ret returns its value whatever the command line`` (cmdLine: string) (value: int) =
        let p = ret value

        parse p cmdLine
        =! Success value

module Alt =
    [<Fact>]
    let ``Alt succeed if first succeed``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "cmd2" null "desc cmd2"
        
        parse p "cmd1 --arg value"
        =! Success "cmd1"

    [<Fact>]
    let ``Alt succeed if second succeed``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "cmd2" null "desc cmd2"
        
        parse p "cmd2 --arg value"
        =! Success "cmd2"

    [<Fact>]
    let ``Alt fails if both fails``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "cmd2" null "desc cmd2"
        
        parse p "cmd3 --arg value"
        =! Failure [ "Command cmd2 not found" ]

    [<Fact>]
    let ``Alt consumes succeeding token``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        rest p "cmd1 --arg value"
        =! "     --arg value"

    [<Fact>]
    let ``Alt consumes succeeding token (second)``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        rest p "command2 --arg value"
        =! "         --arg value"

    [<Fact>]
    let ``Alt keeps token intact when failing ``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        rest p "other --arg value"
        =! "other --arg value"
    
    [<Fact>]
    let ``Alt aggregates usages``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        usage p "cmd1 --arg value"
        =! [ { Name = "cmd1"; Alt = None; Description = "desc cmd1" }
             { Name = "command2"; Alt = None; Description = "desc cmd2" }]

    [<Fact>]
    let ``Alt alt aggregates usages (second)``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        usage p "command2 --arg value"
        =! [ { Name = "cmd1"; Alt = None; Description = "desc cmd1" }
             { Name = "command2"; Alt = None; Description = "desc cmd2" }]


    [<Fact>]
    let ``Aggregates usages when failing ``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        usage p "other --arg value"
        =! [ { Name = "cmd1"; Alt = None; Description = "desc cmd1" }
             { Name = "command2"; Alt = None; Description = "desc cmd2" }]

module Error =
    [<Property>]
    let ``Error always fails``(cmdLine: string) =
        let p = error "Nope"
        parse p cmdLine
        =! Failure ["Nope"]

    [<Property>]
    let ``Errorf always fails``(NonNull cmdLine) =
        let p = errorf (fun tokens -> Token.toString tokens ) 
        parse p cmdLine
        =! Failure [ cmdLine.TrimEnd(' ')]
