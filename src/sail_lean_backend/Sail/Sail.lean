namespace Sail

/- Placeholder for a future implementation of the state monad some Sail functions use. -/
abbrev SailM := StateM Unit

namespace BitVec

def length {w : Nat} (_ : BitVec w) : Nat := w

def signExtend {w : Nat} (x : BitVec w) (w' : Nat) : BitVec w' :=
  x.signExtend w'

def zeroExtend {w : Nat} (x : BitVec w) (w' : Nat) : BitVec w' :=
  x.zeroExtend w'

def truncate {w : Nat} (x : BitVec w) (w' : Nat) : BitVec w' :=
  x.truncate w'

def truncateLsb {w : Nat} (x : BitVec w) (w' : Nat) : BitVec w' :=
  x.extractLsb' (w - w') w'

def extractLsb {w: Nat} (x: BitVec w) (hi lo: Nat) : BitVec (hi - lo + 1) :=
  x.extractLsb hi lo

def update_subrange {w: Nat} (x: BitVec w) (hi lo: Nat) (y: BitVec (hi - lo + 1)) : BitVec w :=
  sorry

end BitVec
end Sail
