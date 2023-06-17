module Fargo.Parsing
open Fargo
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
    let _, _, usage = arg ValueNone (Token.ofString input)
    [ for u in usage do
        {| Name = u.Name; Alt = u.Alt; Description = u.Description |} ]


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
        =! [ {| Name = "--flag"; Alt = Some "-f"; Description = "description" |} ]

module ReqFlag =
    [<Fact>]
    let ``reqFlag name parsing succeeds``() =
        let f = reqFlag "flag" "f" "description" 
        parse f "cmd --other-arg value --flag -x"
        =! Success true

    [<Fact>]
    let ``reqFlag short name parsing succeeds``() =
        let f = reqFlag "flag" "f" "description" 
        parse f "cmd --other-arg value -f -x"
        =! Success true

    [<Fact>]
    let ``reqFlag token is removed after matching``() =
        let f = reqFlag "flag" "f" "description" 
        rest f "cmd --other-arg value --flag -x"
        =! "cmd --other-arg value        -x"

    [<Fact>]
    let ``reqFlag not matching keep tokens intact``() =
        let f = reqFlag "flag" "f" "description" 
        rest f "cmd --other-arg value --fl -x"
        =! "cmd --other-arg value --fl -x"

    [<Fact>]
    let ``Absence of reqFlag fails``() =
        let f = reqFlag "flag" "f" "description" 
        parse f "cmd --other-arg value -x"
        =! Failure ["Required flag --flag not found"]

    [<Fact>]
    let ``reqFlag usage is returned if it succeeds``() =
        let f = reqFlag "flag" "f" "description" 
        usage f "cmd --other-arg value --flag -x"
        =! [ {| Name = "--flag"; Alt = Some "-f"; Description = "description" |} ]

    [<Fact>]
    let ``reqFlag usage is returned if it fails``() =
        let f = reqFlag "flag" "f" "description" 
        usage f "cmd --other-arg value -x"
        =! [ {| Name = "--flag"; Alt = Some "-f"; Description = "description" |} ]

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
        =! [ {| Name = "command"; Alt = Some "cmd"; Description = "description" |} ]

    [<Fact>]
    let ``cmd usage is returned if it failse``() =
        let c = cmd "command" "cmd" "description" 
        usage c "something --other-arg value --flag -x"
        =! [ {| Name = "command"; Alt = Some "cmd"; Description = "description" |} ]

module Arg =
    [<Fact>]
    let ``arg name parsing succeeds``() =
        let f = arg "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val --arg value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg short name parsing succeeds``() =
        let f =  arg "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val -a value -x"
        =! Success (Some "value")

    [<Fact>]
    let ``arg and its value tokens are removed after matching``() =
        let f =  arg "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val --arg value -x"
        =!  "cmd --other-arg val             -x"

    [<Fact>]
    let ``arg token is removed after if matching only name``() =
        let f =  arg "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val -x --arg"
        =!  "cmd --other-arg val -x"

    [<Fact>]
    let ``arg doesn't change token if not matching``() =
        let f = arg "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of arg succeeds with None``() =
        let f =  arg "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg value -x"
        =! Success None

    [<Fact>]
    let ``arg without it's value fails``() =
        let f =  arg "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]


    [<Fact>]
    let ``arg usage is returned if it succeeds``() =
        let f =  arg "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg val --arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]

    [<Fact>]
    let ``arg usage is returned if absent``() =
        let f =  arg "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]
    
    [<Fact>]
    let ``arg usage is returned if only name``() =
        let f =  arg "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg value -x --arg"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]

module ReqArg =
    [<Fact>]
    let ``req name parsing succeeds``() =
        let f = req "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val --arg value -x"
        =! Success ("value")

    [<Fact>]
    let ``req short name parsing succeeds``() =
        let f =  req "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val -a value -x"
        =! Success ("value")

    [<Fact>]
    let ``req and its value tokens are removed after matching``() =
        let f =  req "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"
    
    [<Fact>]
    let ``req token is removed if matching only name``() =
        let f = req "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val -x --arg"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``req doesn't change token if not matching``() =
        let f = req "arg" "a" "description" Completer.empty
        rest f "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"

    [<Fact>]
    let ``Absence of req fails``() =
        let f =  req "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg value -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``req without it's value fails``() =
        let f =  req "arg" "a" "description" Completer.empty
        parse f "cmd --other-arg val --arg"
        =! Failure ["Argument --arg value is missing"]

    [<Fact>]
    let ``req usage is returned if it succeeds``() =
        let f =  req "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg val --arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]

    [<Fact>]
    let ``req usage is returned if absent``() =
        let f =  req "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]
    
    [<Fact>]
    let ``req usage is returned if only name``() =
        let f =  req "arg" "a" "description" Completer.empty
        usage f "cmd --other-arg value -x --arg"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |} ]


module Applicative =
    [<Fact>]
    let ``Applicative can succeed``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -f -x"
        =! Success( Some "value", true)

    [<Fact>]
    let ``Applicative combine results``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Success( Some "value", false)

    [<Fact>]
    let ``Applicative combine results (other direction)``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val -f -x"
        =! Success( None, true)

    [<Fact>]
    let ``Applicative remove tokens when it succeeds``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd -f --other-arg val --arg value -x"
        =! "cmd    --other-arg val             -x"

    [<Fact>]
    let ``Applicative fails if first fails``() =
        let p =
            cmdLine { 
                let! a = req "arg" "a" "description arg" Completer.empty 
                and! f = reqFlag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd -f --other-arg val -x"
        =! Failure ["Required argument --arg not found"]

    [<Fact>]
    let ``Applicative fails if second fails``() =
        let p =
            cmdLine { 
                let! a = req "arg" "a" "description arg" Completer.empty 
                and! f = reqFlag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val --arg value -x"
        =! Failure ["Required flag --flag not found"]

    [<Fact>]
    let ``Applicative combines failures if both fail``() =
        let p =
            cmdLine { 
                let! a = req "arg" "a" "description arg" Completer.empty 
                and! f = reqFlag "flag" "f" "description flag" 
                return a,f
            }

        parse p "cmd --other-arg val -x"
        =! Failure [ "Required argument --arg not found"
                     "Required flag --flag not found"]

    [<Fact>]
    let ``Applicative remove matched tokens when first fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -f -x"
        =! "cmd --other-arg val    -x"

    [<Fact>]
    let ``Applicative remove matched tokens when second fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val --arg value -x"
        =! "cmd --other-arg val             -x"

    [<Fact>]
    let ``Applicative leaves token intact when nothing matches``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        rest p "cmd --other-arg val -x"
        =! "cmd --other-arg val -x"


    [<Fact>]
    let ``Applicative combine usages when it succeeds``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val --arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description arg" |}
             {| Name = "--flag"; Alt = Some "-f"; Description = "description flag" |}]

    [<Fact>]
    let ``Applicative combine usages when first fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd -f --other-arg val -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description arg" |}
             {| Name = "--flag"; Alt = Some "-f"; Description = "description flag" |}]

    [<Fact>]
    let ``Applicative combine usages when second fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val --arg value -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description arg" |}
             {| Name = "--flag"; Alt = Some "-f"; Description = "description flag" |}] 
    
    [<Fact>]
    let ``Applicative combine usages when both fails``() =
        let p =
            cmdLine { 
                let! a = arg "arg" "a" "description arg" Completer.empty 
                and! f = flag "flag" "f" "description flag" 
                return a,f
            }

        usage p "cmd --other-arg val -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description arg" |}
             {| Name = "--flag"; Alt = Some "-f"; Description = "description flag" |}] 

module Map =
    [<Property>]
    let ``map changes value when it succeeds`` (value: int) =
        let p = req "arg" "a" "description" Completer.empty |> map int
        parse p $"cmd -f --arg %d{value} -x"
        =! Success value

    [<Fact>]
    let ``map keeps failures`` () =
        let p = req "arg" "a" "description" Completer.empty |> map int
        parse p $"cmd -f -x"
        =! Failure [ "Required argument --arg not found" ]

    [<Property>]
    let ``map keeps remaining tokens intact`` (value: int) =
        let p = req "arg" "a" "description" Completer.empty |> map int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``map keeps usage intact`` (value: int) =
        let p = req "arg" "a" "description" Completer.empty |> map int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |}]

module OptMap =
    [<Property>]
    let ``optMap changes value when it succeeds`` (value: int) =
        let p = arg "arg" "a" "description" Completer.empty |> optMap int
        parse p $"cmd -f --arg %d{value} -x"
        =! Success (Some value)
    [<Fact>]
    let ``optMap return None when it doesn't match``() =
        let p = arg "arg" "a" "description" Completer.empty |> optMap int
        parse p $"cmd -f -x"
        =! Success None
    [<Fact>]
    let ``optMap keeps failures`` () =
        let p = arg "arg" "a" "description" Completer.empty |> optMap int
        parse p $"cmd -f -x -a"
        =! Failure [ "Argument --arg value is missing" ]

    [<Property>]
    let ``optMap keeps remaining tokens intact`` (value: int) =
        let p = arg "arg" "a" "description" Completer.empty |> optMap int
        rest p $"cmd -f --arg %10d{value} -x"
        =! "cmd -f                  -x"

    [<Property>]
    let ``optMap keeps usage intact`` (value: int) =
        let p = arg "arg" "a" "description" Completer.empty |> optMap int
        usage p $"cmd -f --arg %d{value} -x"
        =! [ {| Name = "--arg"; Alt = Some "-a"; Description = "description" |}]

