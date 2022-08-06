module Flox.Program

open System
open System.IO
open System.Text

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

let report line where message =
    printfn "[line %i] Error%s: %s" line where message

let error line message =
    report line "" message

[<EntryPoint>]
let main args =
    match args.Length with
    | 0 -> runPrompt ()
    | 1 -> runFile (args.[0])
    | _ ->
        printfn "Usage: flox [script]"
        exit 64
    0
