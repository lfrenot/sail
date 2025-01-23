import Std.Data.DHashMap
namespace Sail

section Regs

variable {Register : Type} {RegisterType : Register → Type} [BEq Register] [LawfulBEq Register] [Hashable Register]

/- The Units are placeholders for a future implementation of the state monad some Sail functions use. -/
abbrev Error := Unit

structure SequentialSate where
  regs : Std.DHashMap Register RegisterType
  mem : Unit
  tags : Unit

inductive RegisterRef : Type → Type where
  | Reg (r: Register) : RegisterRef (RegisterType r)

abbrev PreSailM := EStateM Error (SequentialSate (Register := Register) (RegisterType := RegisterType))

def writeReg (r : Register) (v : RegisterType r) : PreSailM (Register := Register) (RegisterType := RegisterType) Unit :=
  modify fun s => { s with regs := s.regs.insert r v }

def readReg (r : Register) : PreSailM (Register := Register) (RegisterType := RegisterType) (RegisterType r) := do
  let .some s := (← get).regs.get? r
    | throw ()
  pure s

def readRegRef (reg_ref : RegisterRef (Register := Register) (RegisterType := RegisterType) α) : PreSailM (Register := Register) (RegisterType := RegisterType) α := do
  match reg_ref with | .Reg r => readReg r

def writeRegRef (reg_ref : RegisterRef (Register := Register) (RegisterType := RegisterType) α) (a : α) : PreSailM (Register := Register) (RegisterType := RegisterType) Unit := do
  match reg_ref with | .Reg r => writeReg r a

def reg_deref (reg_ref : RegisterRef (Register := Register) (RegisterType := RegisterType) α) := readRegRef reg_ref

end Regs

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
