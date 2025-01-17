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

def extractLsb {w : Nat} (x : BitVec w) (hi lo : Nat) : BitVec (hi - lo + 1) :=
  x.extractLsb hi lo

def updateSubrange' {w : Nat} (x : BitVec w) (start len : Nat) (y : BitVec len) : BitVec w :=
  let mask := ~~~(((BitVec.allOnes len).zeroExtend w) <<< start)
  let y' := mask ||| ((y.zeroExtend w) <<< start)
  x &&& y'

def updateSubrange {w : Nat} (x : BitVec w) (hi lo : Nat) (y : BitVec (hi - lo + 1)) : BitVec w :=
  updateSubrange' x lo _ y

end BitVec
end Sail

structure RegisterRef (regstate regval a : Type) where
  name : String
  read_from : regstate -> a
  write_to : a -> regstate -> regstate
  of_regval : regval -> Option a
  regval_of : a -> regval

def undefined_bitvector (w : Nat) : BitVec w :=
  0

def undefined_int (_ : Unit) : Int :=
  0

def undefined_bit (_ : Unit) : BitVec 1 :=
  undefined_bitvector 1
