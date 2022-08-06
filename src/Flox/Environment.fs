module Flox.Environment

open System.Collections.Generic

open Flox.Error
open Flox.Token

type Environment() =
    let values = Dictionary<string, obj>()

    let undefinedVariableError name =
        RuntimeError(name, sprintf "Undefined variable '%s'." name.Lexeme)

    member _.Define(name, value) =
        values[name.Lexeme] <- value

    member _.Get(name) =
        if values.ContainsKey(name.Lexeme) then values[name.Lexeme]
        else raise <| undefinedVariableError name

    member _.Assign(name, value) =
        if values.ContainsKey(name.Lexeme) then
            values[name.Lexeme] <- value
        else raise <| undefinedVariableError name
