import Out.Sail.Sail

open Sail

def foo : (BitVec 16) :=
  let z := (HOr.hOr (0xFFFF : (BitVec 16)) (0xABCD : (BitVec 16)))
  (HAnd.hAnd (0x0000 : (BitVec 16)) z)

def initialize_registers : Unit :=
  ()

