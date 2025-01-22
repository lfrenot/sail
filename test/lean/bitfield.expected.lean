import Out.Sail.Sail

open Sail

def cr_type := (BitVec 8)


def Register (T : Type) := T
abbrev Regstate := Unit
def register_lookup {T : Type} (reg : Register T) (_ : Regstate) : T := reg
def register_set {T : Type} (_ : Register T) : T -> Regstate -> Regstate := fun _ _ => ()
abbrev SailM := PreSailM Regstate
def read_reg {T : Type} : Register T -> SailM T := @Sail.read_reg _ T _ @register_lookup
def write_reg {T : Type} : Register T -> T -> SailM Unit := @Sail.write_reg _ T _ @register_set
def reg_deref {T : Type} : RegisterRef Register T → SailM T := @Sail.reg_deref _ T _ @read_reg

def undefined_cr_type (lit : Unit) : SailM (BitVec 8) := do
  sorry

def Mk_cr_type (v : (BitVec 8)) : (BitVec 8) :=
  v

def _get_cr_type_bits (v : (BitVec 8)) : (BitVec 8) :=
  (Sail.BitVec.extractLsb v (HSub.hSub 8 1) 0)

def _update_cr_type_bits (v : (BitVec 8)) (x : (BitVec 8)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v (HSub.hSub 8 1) 0 x)

def _set_cr_type_bits (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 8)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def _get_cr_type_CR0 (v : (BitVec 8)) : (BitVec 4) :=
  (Sail.BitVec.extractLsb v 7 4)

def _update_cr_type_CR0 (v : (BitVec 8)) (x : (BitVec 4)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v 7 4 x)

def _set_cr_type_CR0 (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 4)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def _get_cr_type_CR1 (v : (BitVec 8)) : (BitVec 2) :=
  (Sail.BitVec.extractLsb v 3 2)

def _update_cr_type_CR1 (v : (BitVec 8)) (x : (BitVec 2)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v 3 2 x)

def _set_cr_type_CR1 (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 2)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def _get_cr_type_CR3 (v : (BitVec 8)) : (BitVec 2) :=
  (Sail.BitVec.extractLsb v 1 0)

def _update_cr_type_CR3 (v : (BitVec 8)) (x : (BitVec 2)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v 1 0 x)

def _set_cr_type_CR3 (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 2)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def _get_cr_type_GT (v : (BitVec 8)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 6 6)

def _update_cr_type_GT (v : (BitVec 8)) (x : (BitVec 1)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v 6 6 x)

def _set_cr_type_GT (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 1)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def _get_cr_type_LT (v : (BitVec 8)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 7 7)

def _update_cr_type_LT (v : (BitVec 8)) (x : (BitVec 1)) : (BitVec 8) :=
  (Sail.BitVec.updateSubrange v 7 7 x)

def _set_cr_type_LT (r_ref : RegisterRef Register (BitVec 8)) (v : (BitVec 1)) : SailM Unit := do
  let r ← (reg_deref r_ref)
  sorry /- deref -/

def initialize_registers : Unit :=
  ()

