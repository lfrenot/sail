import Out.Sail.Sail

open Sail

structure My_struct where
  field1 : Int
  field2 : Int

def undefined_My_struct (lit : Unit) : SailM My_struct :=
  return sorry

def initialize_registers : Unit :=
  ()

