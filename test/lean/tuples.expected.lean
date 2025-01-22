import Out.Sail.Sail

open Sail

def tuple1 : (Int × Int × ((BitVec 2) × Unit)) :=
  (3, 5, ((0b10 : (BitVec 2)), ()))

def tuple2 : SailM (Int × Int) := do
  (pure ((← sorry), (← sorry)))

def initialize_registers : Unit :=
  ()

