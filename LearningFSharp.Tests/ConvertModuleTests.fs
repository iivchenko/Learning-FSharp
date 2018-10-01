namespace LearningFSharp.Tests
    
open FsUnit
open NUnit.Framework
open LearningFSharp.ConvertModule
open System.Text.RegularExpressions
open System

[<TestFixture>]
type ConvertModuleTests() =
    
    [<TestCase(null)>]
    [<TestCase("")>]
    [<TestCase("2.1")>]
    [<TestCase("2,1")>]
    member this.``TryInt. Bad input. None``(input : string) =
        match input with 
        | TryInt _-> failwith <| "Input '" + input + "' should be BAD!"             
        | _ -> ()

    [<TestCase("0", 0)>]
    [<TestCase("1", 1)>]
    [<TestCase("-1", -1)>]
    [<TestCase("-666", -666)>]
    [<TestCase("666", 666)>]
    member this.``TryInt. Good input. Some``(input : string, expected : int) =
         match input with 
        | TryInt x -> x |> should equal expected
        | _ -> failwith <| "Input '" + input + "' should be GOOD!"

    [<TestCase(null)>]
    [<TestCase("")>]
    member this.``TryFloat. Bad input. None``(input : string) =
        match input with 
        | TryDouble _-> failwith <| "Input '" + input + "' should be BAD!"
        | _ -> ()

    [<TestCase("0", 0)>]
    [<TestCase("1", 1)>]
    [<TestCase("-1", -1)>]
    [<TestCase("-666", -666)>]
    [<TestCase("666", 666)>]
    [<TestCase("1.1", 1.1f)>]
    [<TestCase("-1.1", -1.1f)>]
    [<TestCase("-666.1", -666.1f)>]
    [<TestCase("666.1", 666.1f)>]
    member this.``TryFloat. Good input. Some``(input : string, expected : float) =
        match input with 
        | TryDouble x -> x |> should equal expected
        | _ -> failwith <| "Input '" + input + "' should be GOOD!"
            
    [<TestCase("")>]
    [<TestCase("\"")>]
    [<TestCase("Bad")>]
    member this.``TryString. Bad input. None``(input : string) =
        match input with 
        | TryString _-> failwith <| "Input '" + input + "' should be BAD!"
        | _ -> ()

    [<TestCase("\"\"", "")>]
    [<TestCase("\" \"", " ")>]
    [<TestCase("\"1\"", "1")>]
    [<TestCase("\"q3u091834p9 toiasdjf p98q234 09ie r[901u43-9 \"", "q3u091834p9 toiasdjf p98q234 09ie r[901u43-9 ")>]
    member this.``TryString. Good input. Some``(input : string, expected : string) =
        match input with 
        | TryString x -> x |> should equal expected
        | _ -> failwith <| "Input '" + input + "' should be GOOD!"

