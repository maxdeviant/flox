module Flox.Interpreter

open System

open Flox.Expr
open Flox.Token

type RuntimeError(token: Token, message: string) =
    inherit Exception(message)

let rec evaluate expr =
    let isEqual (a: obj) (b: obj) =
        match a, b with
        | null, null -> true
        | null, _
        | _, null -> false
        | _ -> a.Equals(b)


    let isTruthy (value: obj) =
        match value with
        | null -> false
        | :? Boolean as bool -> bool
        | _ -> true

    let checkNumberOperand operator (operand: obj) =
        match operand with
        | :? double -> operand |> unbox<double>
        | _ -> raise <| RuntimeError(operator, "Operand must be a number.")

    let checkNumberOperands operator (left: obj) (right: obj) =
        match left, right with
        | :? double, :? double -> (left |> unbox<double>, right |> unbox<double>)
        | _ -> raise <| RuntimeError(operator, "Operands must be numbers.")

    match expr with
    | Literal value -> value
    | Grouping expr -> evaluate expr
    | Unary(operator, right) ->
        let right = evaluate right

        match operator.Type with
        | Bang -> not (isTruthy right)
        | Minus ->
            let right = checkNumberOperand operator right
            -right
        | _ -> failwith "Unreachable."
    | Binary(left, operator, right) ->
        let left = evaluate left
        let right = evaluate right

        match operator.Type with
        | Greater
        | GreaterEqual
        | Less
        | LessEqual ->
            let checkNumberOperands' = checkNumberOperands operator

            let operator =
                match operator.Type with
                | Greater -> (>)
                | GreaterEqual -> (>=)
                | Less -> (<)
                | LessEqual -> (<=)
                | _ -> failwith "Unreachable."

            let (left, right) = checkNumberOperands' left right

            operator left right |> box
        | Minus
        | Slash
        | Star ->
            let checkNumberOperands' = checkNumberOperands operator

            let operator =
                match operator.Type with
                | Minus -> (-)
                | Slash -> (/)
                | Star -> (*)
                | _ -> failwith "Unreachable."

            let (left, right) = checkNumberOperands' left right

            operator left right |> box
        | Plus ->
            match left, right with
            | :? double, :? double -> unbox<double> left + unbox<double> right |> box
            | :? string, :? string -> unbox<string> left + unbox<string> right |> box
            | _ -> raise <| RuntimeError(operator, "Operands must be two numbers or two strings.")
        | _ -> failwith "Unreachable."
