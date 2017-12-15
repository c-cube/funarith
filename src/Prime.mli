
module type S = Prime_intf.S

module Make(I : Int.S) : S with module Int = I
