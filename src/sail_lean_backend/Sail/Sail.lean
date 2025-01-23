import Std.Data.DHashMap
namespace Sail

/- The Units are placeholders for a future implementation of the state monad some Sail functions use. -/
abbrev Error := Unit

structure SequentialSate (Register : Type) (RegisterType : Register → Type) [BEq Register] [Hashable Register] where
  regs : Std.DHashMap Register RegisterType
  mem : Unit
  tags : Unit

structure RegisterRef (Register : Type) (RegisterType : Register → Type) where
  reg : Register

abbrev PreSailM (Register : Type) (RegisterType : Register → Type) [BEq Register] [Hashable Register] := EStateM Error (SequentialSate Register RegisterType)

def write_reg {RegisterType : Register → Type} [BEq Register] [Hashable Register] (r : Register) (v : RegisterType r) : PreSailM Register RegisterType Unit :=
  modify fun s => { s with regs := s.regs.insert r v }

def read_reg {RegisterType : Register → Type} [BEq Register] [LawfulBEq Register] [Hashable Register] (r : Register) : PreSailM Register RegisterType (RegisterType r) := do
  let .some s := (← get).regs.get? r
    | throw ()
  pure s

def reg_deref {RegisterType : Register → Type} [BEq Register] [LawfulBEq Register] [Hashable Register] (reg_ref : RegisterRef Register RegisterType) : PreSailM Register RegisterType (RegisterType reg_ref.reg) := do
  read_reg reg_ref.reg

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
