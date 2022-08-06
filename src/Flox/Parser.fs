module Flox.Parser

open System.Collections.Generic

open Flox.Expr
open Flox.Stmt
open Flox.Token

exception ParseError of unit

type Parser(tokens: List<Token>) =
    let mutable current = 0;

    let peek () = tokens[current]

    let previous () = tokens[current - 1]

    let error token message =
        Error.errorAtToken token message
        ParseError()

    let isAtEnd () =
        let peekedToken = peek ()
        peekedToken.Type = Eof

    let advance () =
        if not <| isAtEnd () then current <- current + 1
        previous ()

    let advance' = advance >> ignore

    let synchronize () =
        advance' ()

        let mutable quit = false
        while not quit && not <| isAtEnd () do
            let previousToken = previous ()
            if previousToken.Type = Semicolon then quit <- true
            else
                let peekedToken = peek ()
                match peekedToken.Type with
                | Class
                | Fun
                | Var
                | For
                | If
                | While
                | Print
                | Return -> quit <- true
                | _ -> ()

                if not quit then advance' ()

    let check ty =
        if isAtEnd () then false
        else
            let peekedToken = peek ()
            peekedToken.Type = ty

    let match' types =
        types
        |> List.exists (fun ty ->
            if check ty then
                advance' ()
                true
            else false)

    let consume ty message =
        if check ty then advance ()
        else raise <| error (peek ()) message

    let consume' ty message = consume ty message |> ignore

    let binaryExprParser types operand () =
        let mutable expr = operand ()

        while match' types do
            let operator = previous ()
            let right = operand ()
            expr <- Binary(expr, operator, right)

        expr

    let rec primary () =
        match true with
        | _ when match' [False] -> Literal false
        | _ when match' [True] -> Literal true
        | _ when match' [Nil] -> Literal null
        | _ when match' [Number; String] ->
            let previousToken = previous ()
            Literal previousToken.Literal
        | _ when match' [Identifier] -> Variable <| previous ()
        | _ when match' [LeftParen] ->
            let expr = expression ()
            ignore <| consume RightParen "Expected ')' after expression."
            Grouping expr
        | _ -> raise <| error (peek ()) "Expected expression."

    and unary () =
        if match' [Bang; Minus] then
            let operator = previous ()
            let right = unary ()
            Unary(operator, right)
        else primary ()

    and factor = binaryExprParser [Slash; Star] unary

    and term = binaryExprParser [Minus; Plus] factor

    and comparison = binaryExprParser [Greater; GreaterEqual; Less; LessEqual] term

    and equality = binaryExprParser [BangEqual; EqualEqual] comparison

    and expression = equality

    let printStatement () =
        let value = expression ()
        consume' Semicolon "Expected ';' after value."
        Stmt.Print value

    let varDeclaration () =
        let name = consume Identifier "Expected variable name."

        let initializer =
            if match' [Equal] then Some <| expression ()
            else None

        consume' Semicolon "Expected ';' after variable declaration."
        Stmt.Var(name, initializer)

    let expressionStatement () =
        let expr = expression ()
        consume' Semicolon "Expected ';' after expression."
        Expression expr

    let statement () =
        if match' [Print] then printStatement ()
        else expressionStatement()

    let declaration () =
        try
            if match' [Var] then varDeclaration ()
            else statement ()
            |> Some
        with
        | :? ParseError as error ->
            synchronize ()
            None

    member _.Parse() =
        let statements = List<Stmt>()

        while not <| isAtEnd() do
            match declaration () with
            | Some declaration ->
                statements.Add(declaration)
            | None -> ()
        
        statements
