module Flox.Stmt

open Flox.Expr
open Flox.Token

type Stmt =
    | Block of Stmt list
    | Expression of Expr
    | Function of FunctionDecl
    | If of condition: Expr * thenBranch: Stmt * elseBranch: Stmt option
    | Print of Expr
    | Var of name: Token * initializer: Expr option
    | While of condition: Expr * body: Stmt

and FunctionDecl = FunctionDecl of name: Token * parameters: Token list * body: Stmt list
