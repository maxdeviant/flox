module Flox.Interpreter

open System

open Flox.Expr
open Flox.Token

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

    match expr with
    | Literal value -> value
    | Grouping expr -> evaluate expr
    | Unary(operator, right) ->
        let right = evaluate right

        match operator.Type with
        | Bang -> not (isTruthy right)
        | Minus -> -(right |> unbox<double>)
        | _ -> failwith "Unreachable."
    | Binary(left, operator, right) ->
        let left = evaluate left
        let right = evaluate right

        match operator.Type with
        | Greater
        | GreaterEqual
        | Less
        | LessEqual ->
            let operator =
                match operator.Type with
                | Greater -> (>)
                | GreaterEqual -> (>=)
                | Less -> (<)
                | LessEqual -> (<=)
                | _ -> failwith "Unreachable."

            let left = left |> unbox<double>
            let right = right |> unbox<double>

            operator left right |> box
        | Minus
        | Slash
        | Star ->
            let operator =
                match operator.Type with
                | Minus -> (-)
                | Slash -> (/)
                | Star -> (*)
                | _ -> failwith "Unreachable."

            let left = left |> unbox<double>
            let right = right |> unbox<double>

            operator left right |> box
        | Plus ->
            match left, right with
            | :? double, :? double -> unbox<double> left + unbox<double> right |> box
            | :? string, :? string -> unbox<string> left + unbox<string> right |> box
            | _ -> failwith "Unreachable."
        | _ -> failwith "Unreachable."
