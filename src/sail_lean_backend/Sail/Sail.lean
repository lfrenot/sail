namespace Sail

/- The Units are placeholders for a future implementation of the state monad some Sail functions use. -/
abbrev Error := Unit

structure SequentialSate ( Regs : Type ) where
  regs : Regs
  mem : Unit
  tags : Unit

abbrev PreSailM ( Regs: Type ) := EStateM Error (SequentialSate Regs)

structure RegisterRef (Register : Type -> Type) (T : Type) where
  reg : Register T

def read_reg {Register : Type -> Type} (register_lookup : ∀ T, Register T -> Regstate -> T) (reg : Register S) : PreSailM Regstate S := do
  let r ← get
  return register_lookup _ reg r.regs

def write_reg {Register : Type -> Type} (register_set : ∀ T, Register T -> T -> Regstate -> Regstate) (reg : Register S) (s : S) : PreSailM Regstate Unit := do
  let r ← get
  set { r with regs := register_set _ reg s r.regs }
  return ()

def reg_deref {Register : Type → Type} (read_reg : ∀ T, Register T → PreSailM Regstate T) (reg_ref : RegisterRef Register T) : PreSailM Regstate T := do
  read_reg _ reg_ref.reg

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
