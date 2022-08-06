module Flox.Environment

open System.Collections.Generic

open Flox.Error
open Flox.Token

type Environment(enclosing: Environment option) =
    let values = Dictionary<string, obj>()

    let undefinedVariableError name =
        RuntimeError(name, sprintf "Undefined variable '%s'." name.Lexeme)

    new() = Environment(None)

    member _.Define(name, value) =
        values[name.Lexeme] <- value

    member _.Get(name) =
        if values.ContainsKey(name.Lexeme) then values[name.Lexeme]
        else
            match enclosing with
            | Some enclosing -> enclosing.Get(name)
            | None -> raise <| undefinedVariableError name

    member _.Assign(name, value) =
        if values.ContainsKey(name.Lexeme) then
            values[name.Lexeme] <- value
        else
            match enclosing with
            | Some enclosing -> enclosing.Assign(name, value)
            | None -> raise <| undefinedVariableError name
