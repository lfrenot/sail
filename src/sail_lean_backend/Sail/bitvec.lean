namespace Sail
namespace BitVec

def length {w: Nat} (_: BitVec w): Nat := w

def signExtend {w: Nat} (x: BitVec w) (w': Nat) : BitVec w' :=
  x.signExtend w'

def zeroExtend {w: Nat} (x: BitVec w) (w': Nat) : BitVec w' :=
  x.zeroExtend w'

def truncate {w: Nat} (x: BitVec w) (w': Nat) : BitVec w' :=
  x.truncate w'

def truncateLSB {w: Nat} (x: BitVec w) (w': Nat) : BitVec w' :=
  x.extractLsb' 0 w'

def extractLsb {w: Nat} (x: BitVec w) (hi lo: Nat) : BitVec (hi - lo + 1) :=
  x.extractLsb hi lo

def update_subrange' {w: Nat} (x: BitVec w) (start len: Nat) (y: BitVec len) : BitVec w :=
  let mask := ~~~(((BitVec.allOnes len).zeroExtend w) <<< start)
  let y' := mask ||| ((y.zeroExtend w) <<< start)
  x &&& y'

def update_subrange {w: Nat} (x: BitVec w) (hi lo: Nat) (y: BitVec (hi - lo + 1)) : BitVec w :=
  update_subrange' x lo _ y

end BitVec
end Sail
