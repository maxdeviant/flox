module Flox.Stmt

open Flox.Expr

type Stmt =
    | Expression of Expr
    | Print of Expr
