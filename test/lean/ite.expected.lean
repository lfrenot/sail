import Out.Sail.Sail

open Sail

inductive Register : Type where
  | B
  | R
  deriving DecidableEq, Hashable
open Register

abbrev RegisterType : Register → Type
  | .B => Bool
  | .R => Nat

abbrev SailM := PreSailM RegisterType

open RegisterRef
instance : Inhabited (RegisterRef RegisterType Bool) where
  default := .Reg B
instance : Inhabited (RegisterRef RegisterType Nat) where
  default := .Reg R

/-- Type quantifiers: n : Int, 0 ≤ n -/
def elif (n : Nat) : (BitVec 1) :=
  if (Eq n 0)
  then 1#1
  else if (Eq n 1)
       then 1#1
       else 0#1

/-- Type quantifiers: n : Int, 0 ≤ n -/
def monadic_in_out (n : Nat) : SailM Nat := do
  if (← readReg B)
    then writeReg R n
    else (pure ())
  readReg R

/-- Type quantifiers: n : Int, 0 ≤ n -/
def monadic_lines (n : Nat) : SailM Unit := do
  let b := (Eq n 0)
  if b
  then writeReg R n
       writeReg B b
  else writeReg B b

def initialize_registers : SailM Unit := do
  writeReg R sorry
  writeReg B sorry

