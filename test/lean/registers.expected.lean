import Out.Sail.Sail

open Sail

inductive Register : Type where
  | BIT
  | NAT
  | BOOL
  | INT
  | R1
  | R0
  deriving DecidableEq, Hashable
open Register

abbrev RegisterType : Register → Type
  | .BIT => (BitVec 1)
  | .NAT => Nat
  | .BOOL => Bool
  | .INT => Int
  | .R1 => (BitVec 64)
  | .R0 => (BitVec 64)

abbrev SailM := PreSailM Register RegisterType

def initialize_registers : SailM Unit := do
  write_reg R0 sorry
  write_reg R1 sorry
  write_reg INT sorry
  write_reg BOOL sorry
  write_reg NAT sorry
  write_reg BIT sorry

