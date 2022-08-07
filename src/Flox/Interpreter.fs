module Flox.Interpreter

open System
open System.Collections.Generic

open Flox.Environment
open Flox.Error
open Flox.Expr
open Flox.Stmt
open Flox.Token

let stringify (value: obj) =
    match value with
    | null -> "nil"
    | :? bool ->
        match unbox<bool> value with
        | true -> "true"
        | false -> "false"
    | :? double ->
        let text = value.ToString()
        if text.EndsWith(".0") then
            text[0..text.Length - 2 - 1]
        else text
    | _ -> value.ToString()

let isTruthy (value: obj) =
    match value with
    | null -> false
    | :? Boolean as bool -> bool
    | _ -> true

type Interpreter() as interpreter =
    let globals = Environment()
    let mutable environment = globals

    do
        globals.Define("clock",
            {
                new obj() with
                    override _.ToString() = "<native fn>"

                interface IFloxCallable with
                    member _.Arity = 0
                    member _.Call interpreter _arguments =
                        let timeSinceUnixEpoch = DateTime.UtcNow - DateTime.UnixEpoch
                        timeSinceUnixEpoch.TotalSeconds
            })

    let rec evaluate expr =
        let isEqual (a: obj) (b: obj) =
            match a, b with
            | null, null -> true
            | null, _
            | _, null -> false
            | _ -> a.Equals(b)

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
        | Variable name -> environment.Get(name)
        | Assign(name, value) ->
            let value = evaluate value
            environment.Assign(name, value)
            value
        | Logical(left, operator, right) ->
            let left = evaluate left

            match operator.Type, left |> isTruthy with
            | Or, true -> left
            | And, false -> left
            | _ -> evaluate right
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
        | Call(callee, closingParen, arguments) ->
            let callee = evaluate callee

            let arguments = arguments |> List.map evaluate

            match callee with
            | :? IFloxCallable as fn ->
                let argumentCount = arguments |> List.length
                if argumentCount <> fn.Arity then
                    raise <| RuntimeError(closingParen, sprintf "Expected %i arguments but got %i." fn.Arity argumentCount)

                fn.Call interpreter arguments
            | _ -> raise <| RuntimeError(closingParen, "Can only call functions and classes.")

    let rec executeBlock statements newEnvironment =
        let previous = environment;

        try
            environment <- newEnvironment;

            for statement in statements do
                execute statement
        finally
            environment <- previous

    and execute stmt =
        match stmt with
        | Block statements ->
            let newEnvironment = Environment(Some environment)
            executeBlock statements newEnvironment
        | Expression expr -> expr |> evaluate |> ignore
        | Stmt.If(condition, thenBranch, elseBranch) ->
            if condition |> evaluate |> isTruthy then
                execute thenBranch
            else
                match elseBranch with
                | Some elseBranch -> execute elseBranch
                | None -> ()
        | Stmt.While(condition, body) ->
            while condition |> evaluate |> isTruthy do
                execute body
        | Stmt.Print expr ->
            let value = evaluate expr
            printfn "%s" <| stringify value
        | Stmt.Var(name, initializer) ->
            let value =
                match initializer with
                | Some initializer -> evaluate initializer
                | None -> null
            
            environment.Define(name, value)

    member _.Interpret(statements: List<Stmt>) =
        try
            for statement in statements do
                execute statement
        with
        | :? RuntimeError as error ->
            Error.runtimeError error

and IFloxCallable =
    abstract member Arity: int
    abstract member Call: interpreter: Interpreter -> arguments: obj list -> obj
