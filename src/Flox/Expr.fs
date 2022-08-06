module Flox.Expr

open System

open Flox.Token

type Expr =
    | Binary of left: Expr * operator: Token * right: Expr
    | Grouping of Expr
    | Literal of Object
    | Unary of operator: Token * right: Expr
