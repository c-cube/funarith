
module type S = Prime_intf.S

module Make(I : Int.FULL) : S with module Z = I
