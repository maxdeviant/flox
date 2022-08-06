module Flox.Program

open System
open System.IO
open System.Text

open Flox.Expr
open Flox.Parser
open Flox.Scanner
open Flox.Token

let run source =
    let scanner = Scanner(source)
    let tokens = scanner.ScanTokens()

    let parser = Parser(tokens)
    let statements = parser.Parse()

    match Error.hadError with
    | true -> ()
    | false -> Interpreter.interpret statements

let runFile path =
    let bytes = File.ReadAllBytes(path)
    run <| Encoding.UTF8.GetString(bytes)
    if Error.hadError then exit 65
    if Error.hadRuntimeError then exit 70

let runPrompt () =
    let mutable quitPrompt = false
    while not quitPrompt do
        printf "> "
        let line = Console.ReadLine()
        if String.IsNullOrEmpty line then quitPrompt <- true
        else
            run line
            Error.hadError <- false

[<EntryPoint>]
let main args =
    match args.Length with
    | 0 -> runPrompt ()
    | 1 -> runFile (args[0])
    | _ ->
        printfn "Usage: flox [script]"
        exit 64
    0
