import Out.Sail.Sail

open Sail


def Register (T : Type) := T
abbrev Regstate := Unit
def register_lookup {T : Type} (reg : Register T) (_ : Regstate) : T := reg
def register_set {T : Type} (_ : Register T) : T → Regstate → Regstate := fun _ _ => ()
abbrev SailM := PreSailM Regstate
def read_reg {T : Type} : Register T → SailM T := @Sail.read_reg _ T _ @register_lookup
def write_reg {T : Type} : Register T → T → SailM Unit := @Sail.write_reg _ T _ @register_set
def reg_deref {T : Type} : RegisterRef Register T → SailM T := @Sail.reg_deref _ T _ @read_reg

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

