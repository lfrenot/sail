import Out.Sail.Sail

open Sail

structure My_struct where
  field1 : Int
  field2 : (BitVec 1)


def undefined_My_struct (lit : Unit) : SailM My_struct := do
  let w__0 := (undefined_int ())
  let w__1 := (undefined_bit ())
  return {field1 := w__0,field2 := w__1}

def struct_field2 (s : My_struct) : (BitVec 1) :=
  s.field2

def struct_update_field2 (s : My_struct) (b : (BitVec 1)) : My_struct :=
  {s with field2 := b}

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

def initialize_registers : Unit :=
  ()

