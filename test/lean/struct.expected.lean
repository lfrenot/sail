import Out.Sail.Sail

open Sail

structure My_struct where
  field1 : Int
  field2 : (BitVec 1)

inductive Register : Type where
  | r
  deriving DecidableEq, Hashable
open Register

abbrev RegisterType : Register → Type
  | .r => My_struct

abbrev SailM := PreSailM RegisterType

open RegisterRef
instance : Inhabited (RegisterRef RegisterType My_struct) where
  default := .Reg r

def undefined_My_struct (lit : Unit) : SailM My_struct := do
  (pure { field1 := (← sorry)
          field2 := (← sorry) })

def struct_field2 (s : My_struct) : (BitVec 1) :=
  s.field2

def struct_update_field2 (s : My_struct) (b : (BitVec 1)) : My_struct :=
  { s with field2 := b }

/-- Type quantifiers: i : Int -/
def struct_update_both_fields (s : My_struct) (i : Int) (b : (BitVec 1)) : My_struct :=
  { s with field1 := i, field2 := b }

/-- Type quantifiers: i : Int -/
def mk_struct (i : Int) (b : (BitVec 1)) : My_struct :=
  { field1 := i
    field2 := b }

def undef_struct (x : (BitVec 1)) : SailM My_struct := do
  ((undefined_My_struct ()) : SailM My_struct)

def initialize_registers : SailM Unit := do
  writeReg r (← (undefined_My_struct ()))

