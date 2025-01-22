import Out.Sail.Sail

open Sail


def Register (T : Type) := T
abbrev Regstate := Unit
def register_lookup {T : Type} (reg : Register T) (_ : Regstate) : T := reg
def register_set {T : Type} (_ : Register T) : T -> Regstate -> Regstate := fun _ _ => ()
abbrev SailM := PreSailM Regstate
def read_reg {T : Type} : Register T -> SailM T := @Sail.read_reg _ T _ @register_lookup
def write_reg {T : Type} : Register T -> T -> SailM Unit := @Sail.write_reg _ T _ @register_set
def reg_deref {T : Type} : RegisterRef Register T → SailM T := @Sail.reg_deref _ T _ @read_reg

def foo (y : Unit) : Unit :=
  y

def initialize_registers : Unit :=
  ()

