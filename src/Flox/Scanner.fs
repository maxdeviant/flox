module Flox.Scanner

open System
open System.Collections.Generic

open Flox.Token

let keywords =
    Map.empty
    |> Map.add "and" And
    |> Map.add "class" Class
    |> Map.add "else" Else
    |> Map.add "false" False
    |> Map.add "for" For
    |> Map.add "fun" Fun
    |> Map.add "if" If
    |> Map.add "nil" Nil
    |> Map.add "or" Or
    |> Map.add "print" Print
    |> Map.add "return" Return
    |> Map.add "super" Super
    |> Map.add "this" This
    |> Map.add "true" True
    |> Map.add "var" Var
    |> Map.add "while" While

type Scanner(source: string) =
    let tokens = List<Token>()

    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let isAtEnd () = current >= source.Length

    let addToken ty literal =
        let token =
            {
                Type = ty
                Lexeme = source[start..current - 1]
                Literal = literal
                Line = line
            }

        tokens.Add(token)

    let addToken' ty = addToken ty null

    let peek () =
        if isAtEnd () then Char.MinValue
        else source[current]
        
    let peekNext () =
        if current + 1 >= source.Length then Char.MinValue
        else source[current + 1]

    let advance () =
        let char = source[current]
        current <- current + 1
        char

    let advance' = advance >> ignore

    let string' () =
        while peek () <> '"' && not <| isAtEnd () do
            if peek () = '\n' then line <- line + 1
            advance' ()

        if isAtEnd () then Error.error line "Unterminated string."
        else
            // The closing `"`.
            advance' ()

            // Trim the surrounding quotes.
            let start = start + 1
            let current = current - 1

            source[start..current - 1]
            |> addToken String

    let isDigit char = char >= '0' && char <= '9'

    let number () =
        while peek () |> isDigit do advance' ()

        // Look for a fractional part.
        if peek () = '.' && peekNext () |> isDigit then
            // Consume the `.`.
            advance' ()

            while peek () |> isDigit do advance' ()

        Double.Parse(source[start..current - 1])
        |> addToken Number

    let isAlpha char =
        (char >= 'a' && char <= 'z') ||
        (char >= 'A' && char <= 'Z') ||
        char = '_'

    let isAlphaNumeric char = isAlpha char || isDigit char

    let identifier () =
        while peek () |> isAlphaNumeric do advance' ()

        let text = source[start..current - 1]
        let ty = keywords |> Map.tryFind text |> Option.defaultValue Identifier

        addToken' ty

    let scanToken () =
        let matches expected =
            if isAtEnd () then false
            elif source[current] <> expected then false
            else
                current <- current + 1
                true

        let matchEqual op equalOp = if matches '=' then op else equalOp

        match advance () with
        | '(' -> addToken' LeftParen
        | ')' -> addToken' RightParen
        | '{' -> addToken' LeftBrace
        | '}' -> addToken' RightBrace
        | ',' -> addToken' Comma
        | '.' -> addToken' Dot
        | '-' -> addToken' Minus
        | '+' -> addToken' Plus
        | ';' -> addToken' Semicolon
        | '*' -> addToken' Star
        | '!' -> addToken' <| matchEqual Bang BangEqual
        | '=' -> addToken' <| matchEqual Equal EqualEqual
        | '<' -> addToken' <| matchEqual Less LessEqual
        | '>' -> addToken' <| matchEqual Greater GreaterEqual
        | '/' ->
            if matches '/' then
                while peek () <> '\n' && not <| isAtEnd () do ignore <| advance ()
            else addToken' Slash
        | ' ' | '\r' | '\t' ->
            // Ignore whitespace.
            ()
        | '\n' -> line <- line + 1
        | '"' -> string' ()
        | char ->
            match char with
            | char when char |> isDigit -> number ()
            | char when char |> isAlpha -> identifier ()
            | char -> Error.error line <| sprintf "Unexpected character '%c'." char

    member _.ScanTokens() =
        while not <| isAtEnd () do
            start <- current
            scanToken ()

        let eofToken =
            {
                Type = Eof
                Lexeme = ""
                Literal = null
                Line = line
            }

        tokens.Add(eofToken)
        
        tokens
