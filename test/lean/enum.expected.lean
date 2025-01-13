import Out.Sail.Sail

open Sail

inductive E where | A | B | C
  deriving Inhabited

def undefined_E : SailM E := do
  return (sorry : E)

def initialize_registers : Unit :=
  ()

