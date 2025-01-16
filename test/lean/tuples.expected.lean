import Out.Sail.Sail

open Sail

def tuple1 : (Int × Int × ((BitVec 2) × Unit)) :=
  (3, 5, ((0b10 : (BitVec 2)), ()))

def initialize_registers : Unit :=
  ()

