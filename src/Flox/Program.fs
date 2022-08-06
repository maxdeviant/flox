module Flox.Program

open System
open System.IO
open System.Text

open Flox.Expr
open Flox.Scanner

let mutable hadError = false

let run source =
    let scanner = Scanner(source)
    let tokens = scanner.ScanTokens()

    for token in tokens do
        printfn "%A" token

let runFile path =
    let bytes = File.ReadAllBytes(path)
    run <| Encoding.UTF8.GetString(bytes)
    if hadError then exit 65

let runPrompt () =
    let mutable quitPrompt = false
    while not quitPrompt do
        printf "> "
        let line = Console.ReadLine()
        if String.IsNullOrEmpty line then quitPrompt <- true
        else
            run line
            hadError <- false

[<EntryPoint>]
let main args =
    let expression =
        Binary(
            Unary(
                { Type = Minus; Lexeme = "-"; Literal = null; Line = 1},
                Literal 123
            ),
            { Type = Star; Lexeme = "*"; Literal = null; Line = 1},
            Grouping (Literal 45.67)
        )

    printfn "%s" <| printExpr expression

    match args.Length with
    | 0 -> runPrompt ()
    | 1 -> runFile (args[0])
    | _ ->
        printfn "Usage: flox [script]"
        exit 64
    0
