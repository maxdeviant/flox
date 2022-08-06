module Flox.AstPrinter

open System
open System.Text

open Flox.Expr

let rec printExpr expr : string =
    let parenthesize (name: string) (exprs: Expr list) =
        let builder = StringBuilder()

        builder.Append("(").Append(name) |> ignore

        for expr in exprs do
            builder.Append(" ") |> ignore
            builder.Append(printExpr expr) |> ignore

        builder.Append(")") |> ignore

        builder.ToString()

    match expr with
    | Binary(left, operator, right) -> parenthesize operator.Lexeme [left; right]
    | Grouping expr -> parenthesize "group" [expr]
    | Literal value -> if value = null then "nil" else value.ToString()
    | Unary(operator, right) -> parenthesize operator.Lexeme [right]
