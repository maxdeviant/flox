module Flox.Stmt

open Flox.Expr
open Flox.Token

type Stmt =
    | Expression of Expr
    | Print of Expr
    | Var of name: Token * initializer: Expr option
