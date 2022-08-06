module Flox.Error

open System

open Flox.Token

let mutable hadError = false
let mutable hadRuntimeError = false

let report line where message =
    printfn "[line %i] Error%s: %s" line where message
    hadError <- true

let error line message =
    report line "" message

/// Reports an error at the given token.
let errorAtToken token message =
    let location =
        if token.Type = Eof then " at end"
        else sprintf " at '%s'" token.Lexeme

    report token.Line location message

type RuntimeError(token: Token, message: string) =
    inherit Exception(message)

    member _.Token = token

let runtimeError (error: RuntimeError) =
    printfn "%s\n[line %i]" error.Message error.Token.Line
    hadRuntimeError <- true
