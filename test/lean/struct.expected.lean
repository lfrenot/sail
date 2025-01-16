import Out.Sail.Sail

open Sail

structure My_struct where
  field1 : Int
  field2 : (BitVec 1)

def undefined_My_struct (lit : Unit) : SailM My_struct :=
  return sorry

def struct_field2 (s : My_struct) : (BitVec 1) :=
  s.field2

def struct_update_field2 (s : My_struct) (b : (BitVec 1)) : My_struct :=
  {s with field2 := b}

/-- Type quantifiers: i : Int -/
def mk_struct (i : Int) (b : (BitVec 1)) : My_struct :=
  {field1 := i, field2 := b}

def initialize_registers : Unit :=
  ()

