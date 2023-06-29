module Fargo.Parsing
open Fargo
open Fargo.Operators 
open Xunit
open FsCheck
open FsCheck.Xunit
open DEdge.Diffract

let (=!) (actual:'a) (expected: 'a) = Differ.Assert(expected, actual )


let parse (arg: Arg<'t>) input =
    let result, _, _ = arg ValueNone (Token.ofString input)
    result

let rest (arg: Arg<'t>) input =
    let _, tokens, _ = arg ValueNone (Token.ofString input)
    Token.toString tokens

let usage (arg: Arg<'t>) input =
    let _, _, usages = arg ValueNone (Token.ofString input)
    usages.Options
    


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
        =! [ { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description"; Help = None; Type = UsageType.Arg } ]

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
        =! [ { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]

    [<Fact>]
    let ``reqFlag usage is returned if it fails``() =
        let f = flag "flag" "f" "description" |> reqFlag
        usage f "cmd --other-arg value -x"
        =! [ { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]

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
        =! [ { Name = Some "command"; Alt = Some "cmd"; Value = None; Description = "description"; Help = None; Type = UsageType.Required } ]

    [<Fact>]
    let ``cmd usage is returned if it failse``() =
        let c = cmd "command" "cmd" "description" 
        usage c "something --other-arg value --flag -x"
        =! [ { Name = Some "command"; Alt = Some "cmd"; Value = None; Description = "description"; Help = None; Type = UsageType.Required } ]

module Opt =
    [<Fact>]
    let ``arg name parsing succeeds``() =
        let f = opt "arg" "a" "value" "description"
        parse f "cmd --other-arg val --arg value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg short name parsing succeeds``() =
        let f =  opt "arg" "a" "value" "description"
        parse f "cmd --other-arg val -a value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg and its value tokens are removed after matching``() =
        let f =  opt "arg" "a" "value" "description"
        rest f "cmd --other-arg val --arg value -x"
        =!  "cmd --other-arg val             -x"

    [<Fact>]
    let ``arg token is removed after if matching only name``() =
        let f =  opt "arg" "a" "value" "description"
        rest f "cmd --other-arg val -x --arg"
        =!  "cmd --other-arg val -x"

    [<Fact>]
    let ``arg doesn't change token if not matching``() =
        let f = opt "arg" "a" "value" "description"
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of arg succeeds with None``() =
        let f = opt "arg" "a" "value" "description"
        parse f "cmd --other-arg value -x"
        =! Success None

    [<Fact>]
    let ``arg without it's value fails``() =
        let f = opt "arg" "a" "value" "description"
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]


    [<Fact>]
    let ``arg usage is returned if it succeeds``() =
        let f = opt "arg" "a" "value" "description"
        usage f "cmd --other-arg val --arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg } ]

    [<Fact>]
    let ``arg usage is returned if absent``() =
        let f = opt "arg" "a" "value" "description"
        usage f "cmd --other-arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg } ]
    
    [<Fact>]
    let ``arg usage is returned if only name``() =
        let f = opt "arg" "a" "value" "description"
        usage f "cmd --other-arg value -x --arg"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg } ]

module reqOpt =
    [<Fact>]
    let ``req name parsing succeeds``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        parse f "cmd --other-arg val --arg value -x"
        =! Success ("value")

    [<Fact>]
    let ``req short name parsing succeeds``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        parse f "cmd --other-arg val -a value -x"
        =! Success ("value")

    [<Fact>]
    let ``req and its value tokens are removed after matching``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        rest f "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"
    
    [<Fact>]
    let ``req token is removed if matching only name``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        rest f "cmd --other-arg val -x --arg"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``req doesn't change token if not matching``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of req fails``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        parse f "cmd --other-arg value -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``req without it's value fails``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]

    [<Fact>]
    let ``req usage is returned if it succeeds``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        usage f "cmd --other-arg val --arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]

    [<Fact>]
    let ``req usage is returned if absent``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        usage f "cmd --other-arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]
    
    [<Fact>]
    let ``req usage is returned if only name``() =
        let f = opt "arg" "a" "value" "description" |> reqOpt
        usage f "cmd --other-arg value -x --arg"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]

module Arg =
    [<Fact>]
    let ``arg parsing succeeds``() =
        let f = arg "value" "description"
        parse f "value"
        =! Success (Some "value")

    [<Fact>]
    let ``arg and its value tokens are removed after matching``() =
        let f = arg "value" "description"
        rest f "value"
        =!  ""

    [<Fact>]
    let ``Absence of arg succeeds with None``() =
        let f = arg "value" "description"
        parse f "  "
        =! Success None

    [<Fact>]
    let ``arg usage is returned if it succeeds``() =
        let f = arg "value" "description"
        usage f " value "
        =! [ { Name = None; Alt = None; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg } ]

    [<Fact>]
    let ``arg usage is returned if absent``() =
        let f = arg "value" "description"
        usage f " "
        =! [ { Name = None; Alt = None; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg } ]
    

module reqArg =
    [<Fact>]
    let ``req name parsing succeeds``() =
        let f = arg  "value" "description" |> reqArg
        parse f " value "
        =! Success ("value")


    [<Fact>]
    let ``req and its value tokens are removed after matching``() =
        let f = arg "value" "description" |> reqArg
        rest f " value"
        =! ""
    


    [<Fact>]
    let ``Absence of req fails``() =
        let f = arg "value" "description" |> reqArg
        parse f " "
        =! Failure ["Required argument <value> not found"]

    [<Fact>]
    let ``req usage is returned if it succeeds``() =
        let f = arg "value" "description" |> reqArg
        usage f " value "
        =! [ { Name = None; Alt = None; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]

    [<Fact>]
    let ``req usage is returned if absent``() =
        let f = arg "value" "description" |> reqArg
        usage f "cmd --other-arg value -x"
        =! [ { Name = None; Alt = None; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required } ]
    
module Applicative =
    [<Fact>]
    let ``Applicative can succeed``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -f -x"
        =! Success( Some "value", true)

    [<Fact>]
    let ``Applicative combine results``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Success( Some "value", false)

    [<Fact>]
    let ``Applicative combine results (other direction)``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val -f -x"
        =! Success( None, true)

    [<Fact>]
    let ``Applicative remove tokens when it succeeds``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd -f --other-arg val --arg value -x"
        =! "cmd    --other-arg val             -x"

    [<Fact>]
    let ``Applicative fails if first fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg"  |> reqOpt
                and! f = flag "flag" "f" "description flag"  |> reqFlag
                return a,f
            }

        parse p "cmd -f --other-arg val -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``Applicative fails if second fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg"  |> reqOpt
                and! f = flag "flag" "f" "description flag" |> reqFlag
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Failure ["Required flag --flag not found"]

    [<Fact>]
    let ``Applicative combines failures if both fail``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg"  |> reqOpt
                and! f = flag "flag" "f" "description flag" |> reqFlag
                return a,f
            }

        parse p "cmd --other-arg val -x"
        =! Failure [ "Required argument --arg not found"
                     "Required flag --flag not found"]

    [<Fact>]
    let ``Applicative remove matched tokens when first fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -f -x"
        =! "cmd --other-arg val    -x"

    [<Fact>]
    let ``Applicative remove matched tokens when second fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"

    [<Fact>]
    let ``Applicative leaves token intact when nothing matches``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"


    [<Fact>]
    let ``Applicative combine usages when it succeeds``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val --arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description arg"; Help = None; Type = UsageType.Arg }
             { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description flag"; Help = None; Type = UsageType.Arg }]

    [<Fact>]
    let ``Applicative combine usages when first fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description arg"; Help = None; Type = UsageType.Arg }
             { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description flag"; Help = None; Type = UsageType.Arg }]

    [<Fact>]
    let ``Applicative combine usages when second fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val --arg value -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description arg"; Help = None; Type = UsageType.Arg }
             { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description flag"; Help = None; Type = UsageType.Arg }] 
    
    [<Fact>]
    let ``Applicative combine usages when both fails``() =
        let p =
            fargo { 
                let! a = opt "arg" "a" "value" "description arg" 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description arg"; Help = None; Type = UsageType.Arg }
             { Name = Some "--flag"; Alt = Some "-f"; Value = None; Description = "description flag"; Help = None; Type = UsageType.Arg }] 

module Map =
    [<Property>]
    let ``map changes value when it succeeds`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> map int 
        parse p $"cmd -f --arg %d{value} -x"
        =! Success value

    [<Fact>]
    let ``map keeps failures`` () =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> map int
        parse p $"cmd -f -x"
        =! Failure [ "Required argument --arg not found" ]

    [<Property>]
    let ``map keeps remaining tokens intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> map int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``map keeps usage intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> map int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required }]

module OptMap =
    [<Property>]
    let ``optMap changes value when it succeeds`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optMap int
        parse p $"cmd -f --arg %d{value} -x"
        =! Success (Some value)
    [<Fact>]
    let ``optMap return None when it doesn't match``() =
        let p = opt "arg" "a" "value" "description" |> optMap int
        parse p $"cmd -f -x"
        =! Success None
    [<Fact>]
    let ``optMap keeps failures`` () =
        let p = opt "arg" "a" "value" "description" |> optMap int
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``optMap keeps remaining tokens intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optMap int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``optMap keeps usage intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optMap int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg }]

module Parse =
    let parseInt (input: string) = 
        match System.Int32.TryParse(input, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error "Bad int!"
    [<Property>]
    let ``parse changes value when it succeeds`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        parse p $"cmd -f --arg %d{value} -x"
        =! Success ( value)

    [<Property>]
    let ``parse fail when value cannot be parsed`` () =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        parse p $"cmd -f --arg value -x"
        =! Failure ["Bad int!"] 

    [<Fact>]
    let ``parse keeps failures`` () =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``parse keeps remaining tokens intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``parse don't consume token when parsing fails`` () =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        rest p $"cmd -f --arg value -x"
        =! "cmd -f --arg value -x"

    [<Property>]
    let ``parse keeps usage intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> reqOpt |> Fargo.parse parseInt
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg ||| UsageType.Required }]


module OptParse =
    let parseInt (input: string) = 
        match System.Int32.TryParse(input, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | false, _ -> Error "Bad int!"
    [<Property>]
    let ``optParse changes value when it succeeds`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        parse p $"cmd -f --arg %d{value} -x"
        =! Success (Some value)

    [<Fact>]
    let ``optParse fail when value cannot be parsed`` () =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        parse p $"cmd -f --arg value -x"
        =! Failure ["Bad int!"] 

    [<Fact>]
    let ``optParse return None when it doesn't match``() =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        parse p $"cmd -f -x"
        =! Success None
    [<Fact>]
    let ``optParse keeps failures`` () =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``optParse keeps remaining tokens intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Fact>]
    let ``optParse don't consume token when parsing fails`` () =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        rest p $"cmd -f --arg value -x"
        =! "cmd -f --arg value -x"

    [<Property>]
    let ``optParse keeps usage intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> optParse parseInt
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg }]


module DefaultValue =

    [<Property>]
    let ``defaultValue keep original value when it succeeds`` (value: int) (def: int) =
        let p = opt "arg" "a" "value" "description" |> defaultValue $"%d{def}"
        parse p $"cmd -f --arg %d{value} -x"
        =! Success $"%d{value}"

    [<Property>]
    let ``defaultValue fail when value cannot be parsed`` (value: int) (def: int) =
        let p = opt "arg" "a" "value" "description" |> defaultValue $"%d{def}"
        parse p $"cmd -f -x"
        =! Success $"%d{def}"

    [<Fact>]
    let ``defaultValue keeps failures`` () =
        let p = opt "arg" "a" "value" "description" |> defaultValue "default"
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``defaultValue keeps remaining tokens intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> defaultValue "default"
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``defaultValue don't consume token when parsing fails`` () =
        let p = opt "arg" "a" "value" "description" |> defaultValue "default"
        rest p $"cmd -f -x -a"
        =! "cmd -f -x -a"

    [<Property>]
    let ``defaultValue keeps usage intact`` (value: int) =
        let p = opt "arg" "a" "value" "description" |> defaultValue "default"
        usage p $"cmd -f --arg %d{value} -x"
        =! [ { Name = Some "--arg"; Alt = Some "-a"; Value = Some "value"; Description = "description"; Help = None; Type = UsageType.Arg }]


module Bind =
    
    [<Fact>]
    let ``bind succeeds``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "cmd sub --arg x"
        =! Success("cmd", "sub")

    [<Fact>]
    let ``bind fails if first fails``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "other sub --arg x"
        =! Failure [ "Command cmd not found"]

    [<Fact>]
    let ``bind fails if second fails``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        parse p "cmd other --arg x"
        =! Failure [ "Command sub not found"]

    [<Fact>]
    let ``bind consume both tokens``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "cmd sub --arg x"
        =! "        --arg x"

    [<Fact>]
    let ``bind failing on first consumes no token``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "other sub --arg x"
        =! "other sub --arg x"

    [<Fact>]
    let ``bind failing on second consumes first token``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        rest p "cmd other --arg x"
        =! "    other --arg x"

    [<Fact>]
    let ``bind success returns last usage``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "cmd sub --arg x"
        =! [ { Name = Some "sub"; Alt = None; Value = None; Description = "sub desc"; Help = None; Type = UsageType.Required} ]

    [<Fact>]
    let ``bind failing on first returns first  usage``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "other sub --arg x"
        =! [ { Name = Some "cmd"; Alt = None; Value = None; Description = "desc"; Help = None; Type = UsageType.Required} ]

    [<Fact>]
    let ``bind failing on second returns second usage``() =
        let p =
            fargo {
                let! c = cmd "cmd" null "desc"
                let! s = cmd "sub" null "sub desc"
                return c, s
            }

        usage p "cmd other --arg x"
        =! [ { Name = Some "sub"; Alt = None; Value = None; Description = "sub desc"; Help = None; Type = UsageType.Required} ]

module Ret =
    [<Property>]
    let ``ret returns its value whatever the command line`` (fargo: string) (value: int) =
        let p = ret value

        parse p fargo
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
        =! [ { Name = Some "cmd1"; Alt = None; Value = None; Description = "desc cmd1"; Help = None; Type = UsageType.Required }
             { Name = Some "command2"; Alt = None; Value = None; Description = "desc cmd2"; Help = None; Type = UsageType.Required }]

    [<Fact>]
    let ``Alt alt aggregates usages (second)``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        usage p "command2 --arg value"
        =! [ { Name = Some "cmd1"; Alt = None; Value = None; Description = "desc cmd1"; Help = None; Type = UsageType.Required }
             { Name = Some "command2"; Alt = None; Value = None; Description = "desc cmd2"; Help = None; Type = UsageType.Required }]


    [<Fact>]
    let ``Aggregates usages when failing ``() =
        let p = cmd "cmd1" null "desc cmd1"
                <|> cmd "command2" null "desc cmd2"
        
        usage p "other --arg value"
        =! [ { Name = Some "cmd1"; Alt = None; Value = None; Description = "desc cmd1"; Help = None; Type = UsageType.Required }
             { Name = Some "command2"; Alt = None; Value = None; Description = "desc cmd2"; Help = None; Type = UsageType.Required }]

module Error =
    [<Property>]
    let ``Error always fails``(fargo: string) =
        let p = error "Nope"
        parse p fargo
        =! Failure ["Nope"]

    [<Property>]
    let ``Errorf always fails``(NonNull fargo) =
        let p = errorf (fun tokens -> Token.toString tokens ) 
        parse p fargo
        =! Failure [ fargo.TrimEnd(' ')]
