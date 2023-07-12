module Testing

open System
open System.Text.RegularExpressions

let testLock = obj()

let withStdout f =
    lock testLock (fun _ -> 

        let w = new IO.StringWriter()
        Console.SetOut(w)

        f()

        Regex.Replace(w.ToString(),$"{'\x1b'}\[\d+m","").Split('\n')
        |> Array.map (fun l -> l.TrimEnd())
        |> Array.filter (not << String.IsNullOrEmpty)
        |> String.concat Environment.NewLine
    )





