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

def extern_add : Int :=
  (Int.add 5 4)

def extern_sub : Int :=
  (Int.sub 5 4)

def extern_tdiv : Int :=
  (Int.tdiv 5 4)

def extern_tmod : Int :=
  (Int.tmod 5 4)

def extern_tmod_positive : Int :=
  (Int.tmod 5 4)

def extern_negate : Int :=
  (Int.neg 5)

def extern_mult : Int :=
  (Int.mul 5 4)

def extern_and : Bool :=
  (Bool.and true false)

def extern_and_no_flow : Bool :=
  (Bool.and true false)

def extern_or : Bool :=
  (Bool.or true false)

def extern_eq_bool : Bool :=
  (Eq true false)

def extern_eq_bit : Bool :=
  (Eq 0#1 1#1)

def initialize_registers : Unit :=
  ()

