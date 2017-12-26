
(** Arithmetic expressions *)

module type COEF = Expr_intf.COEF

module type VAR = Expr_intf.VAR
module type FRESH = Expr_intf.FRESH
module type VAR_GEN = Expr_intf.VAR_GEN

module Linear : sig

  module type S = Expr_intf.Linear

  module Make(C : COEF)(Var : VAR)
    : S with module C = C
         and module Var = Var

end
