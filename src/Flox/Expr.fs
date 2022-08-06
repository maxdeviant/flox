module Flox.Expr

open Flox.Token

type Expr =
    | Binary of left: Expr * operator: Token * right: Expr
    | Grouping of Expr
    | Literal of obj
    | Unary of operator: Token * right: Expr
    | Variable of name: Token
