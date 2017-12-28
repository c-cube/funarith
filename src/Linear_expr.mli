
(** Arithmetic expressions *)

module type COEF = Linear_expr_intf.COEF

module type VAR = Linear_expr_intf.VAR
module type FRESH = Linear_expr_intf.FRESH
module type VAR_GEN = Linear_expr_intf.VAR_GEN
module type VAR_EXTENDED = Linear_expr_intf.VAR_EXTENDED

module type S = Linear_expr_intf.S

module Make(C : COEF)(Var : VAR)
  : S with module C = C
       and module Var = Var
       and module Var_map = CCMap.Make(Var)

module Make_var_gen(Var : VAR) : VAR_EXTENDED with type user_var = Var.t
