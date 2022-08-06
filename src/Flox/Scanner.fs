module Flox.Scanner

open System
open System.Collections.Generic

type TokenType =
    // Single-character tokens.
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star

    // One of two character tokens.
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    // Literals.
    | Identifier
    | String
    | Number

    // Keywords.
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While

    | Eof

type Token =
    {
        Type: TokenType
        Lexeme: string
        Literal: Object
        Line: int
    }

type Scanner(source: string) =
    let tokens = List<Token>()

    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let addToken ty literal =
        let token =
            {
                Type = ty
                Lexeme = source.Substring(start, current - start)
                Literal = literal
                Line = line
            }

        tokens.Add(token)

    let addToken' ty = addToken ty null

    let advance () =
        let char = source.[current]
        current <- current + 1
        char

    let scanToken () =
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
        | char -> Error.error line <| sprintf "Unexpected character '%c'." char

    let isAtEnd () =
        current >= source.Length

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
