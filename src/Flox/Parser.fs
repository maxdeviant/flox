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

    let logicalExprParser types operand () =
        let mutable expr = operand ()

        while match' types do
            let operator = previous ()
            let right = operand ()
            expr <- Logical(expr, operator, right)

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

    and call () =
        let finishCall callee =
            let arguments = List<Expr>()

            match check RightParen with
            | false ->
                arguments.Add(expression ())

                while match' [Comma] do
                    if arguments.Count >= 255 then
                        Error.errorAtToken (peek ()) "Can't have more than 255 arguments."

                    arguments.Add(expression ())
            | true -> ()

            let closingParen = consume RightParen "Expected ')' after arguments."

            Call(callee, closingParen, arguments |> List.ofSeq)

        let mutable expr = primary ()

        let mutable finished = false
        while not finished do
            if match' [LeftParen] then
                expr <- finishCall expr
            else finished <- true

        expr

    and unary () =
        if match' [Bang; Minus] then
            let operator = previous ()
            let right = unary ()
            Unary(operator, right)
        else call ()

    and factor = binaryExprParser [Slash; Star] unary

    and term = binaryExprParser [Minus; Plus] factor

    and comparison = binaryExprParser [Greater; GreaterEqual; Less; LessEqual] term

    and equality = binaryExprParser [BangEqual; EqualEqual] comparison

    and logicalAnd = logicalExprParser [And] equality

    and logicalOr = logicalExprParser [Or] logicalAnd

    and assignment () =
        let expr = logicalOr ()

        if match' [Equal] then
            let equals = previous ()
            let value = assignment ()
            
            match expr with
            | Variable(name) -> Assign(name, value)
            | _ ->
                Error.errorAtToken equals "Invalid assignment target."
                expr
        else expr

    and expression = assignment

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

    let rec block () =
        let statements = List<Stmt>()

        while not <| check RightBrace && not <| isAtEnd () do
            match declaration () with
            | Some declaration -> statements.Add(declaration)
            | None -> ()

        consume' RightBrace "Expected '}' after block."
        statements |> List.ofSeq

    and ifStatement () =
        consume' LeftParen "Expected '(' after 'if'."
        let condition = expression ()
        consume' RightParen "Expected ')' after if condition."

        let thenBranch = statement ()
        let elseBranch =
            if match' [Else] then Some <| statement ()
            else None

        Stmt.If(condition, thenBranch, elseBranch)

    and whileStatement () =
        consume' LeftParen "Expected '(' after 'while'."
        let condition = expression ()
        consume' RightParen "Expected ')' after while condition."

        let body = statement ()

        Stmt.While(condition, body)

    and forStatement () =
        consume' LeftParen "Expected '(' after 'for'."

        let initializer =
            match true with
            | _ when match' [Semicolon] -> None
            | _ when match' [Var] -> Some <| varDeclaration ()
            | _ -> Some <| expressionStatement ()

        let condition =
            match check Semicolon with
            | false -> Some <| expression ()
            | true -> None
        consume' Semicolon "Expected ';' after for loop condition."

        let increment =
            match check RightParen with
            | false -> Some <| expression ()
            | true -> None
        consume' RightParen "Expected ')' after for loop clauses."

        statement ()
        |> (fun body ->
            match increment with
            | Some increment -> Block [body; Expression increment]
            | None -> body)
        |> (fun body ->
            let condition = condition |> Option.defaultValue (Literal true)

            Stmt.While(condition, body))
        |> (fun body ->
            match initializer with
            | Some initializer -> Block [initializer; body]
            | None -> body)

    and statement () =
        match true with
        | _ when match' [For] -> forStatement ()
        | _ when match' [If] -> ifStatement ()
        | _ when match' [Print] -> printStatement ()
        | _ when match' [While] -> whileStatement ()
        | _ when match' [LeftBrace] -> Block <| block ()
        | _ -> expressionStatement()

    and declaration () =
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
