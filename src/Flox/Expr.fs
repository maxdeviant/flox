module Flox.Expr

open Flox.Token

type Expr =
    | Assign of name: Token * value: Expr
    | Binary of left: Expr * operator: Token * right: Expr
    | Call of callee: Expr * closingParen: Token * arguments: Expr list
    | Grouping of Expr
    | Literal of obj
    | Logical of left: Expr * operator: Token * right: Expr
    | Unary of operator: Token * right: Expr
    | Variable of name: Token
