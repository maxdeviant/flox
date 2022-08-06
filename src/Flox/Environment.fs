module Flox.Environment

open System.Collections.Generic

open Flox.Error
open Flox.Token

type Environment() =
    let values = Dictionary<string, obj>()

    member _.Define(name, value) =
        values[name.Lexeme] <- value

    member _.Get(name) =
        if values.ContainsKey(name.Lexeme) then values[name.Lexeme]
        else raise <| RuntimeError(name, sprintf "Undefined variable '%s'." name.Lexeme)
