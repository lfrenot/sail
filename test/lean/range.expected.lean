import Out.Sail.Sail

open Sail

/-- Type quantifiers: x : Int, 0 ≤ x ∧ x ≤ 31 -/
def f_int (x : Nat) : Int :=
  0

/-- Type quantifiers: x : Int, 0 ≤ x ∧ x ≤ 31 -/
def f_nat (x : Nat) : Nat :=
  0

/-- Type quantifiers: x : Int, k_n : Int, 0 ≤ x ∧ x ≤ k_n -/
def f_negvar (x : Nat) : Int :=
  x

/-- Type quantifiers: x : Int, k_n : Int, 0 ≤ x ∧ x ≤ k_n -/
def f_nnegvar (x : Nat) : Nat :=
  x

/-- Type quantifiers: x : Int, k_n : Int, k_m : Int, k_n ≤ x ∧ x ≤ k_m -/
def f_unkn (x : Int) : Int :=
  x

def initialize_registers : Unit :=
  ()

