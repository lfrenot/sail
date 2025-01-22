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

def bitvector_eq (x : (BitVec 16)) (y : (BitVec 16)) : Bool :=
  (Eq x y)

def bitvector_neq (x : (BitVec 16)) (y : (BitVec 16)) : Bool :=
  (Ne x y)

def bitvector_len (x : (BitVec 16)) : Nat :=
  (Sail.BitVec.length x)

def bitvector_sign_extend (x : (BitVec 16)) : (BitVec 32) :=
  (Sail.BitVec.signExtend x 32)

def bitvector_zero_extend (x : (BitVec 16)) : (BitVec 32) :=
  (Sail.BitVec.zeroExtend x 32)

def bitvector_truncate (x : (BitVec 32)) : (BitVec 16) :=
  (Sail.BitVec.truncate x 16)

def bitvector_truncateLSB (x : (BitVec 32)) : (BitVec 16) :=
  (Sail.BitVec.truncateLsb x 16)

def bitvector_append (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 32) :=
  (BitVec.append x y)

def bitvector_add (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 16) :=
  (HAdd.hAdd x y)

def bitvector_sub (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 16) :=
  (HSub.hSub x y)

def bitvector_not (x : (BitVec 16)) : (BitVec 16) :=
  (Complement.complement x)

def bitvector_and (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 16) :=
  (HAnd.hAnd x y)

def bitvector_or (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 16) :=
  (HOr.hOr x y)

def bitvector_xor (x : (BitVec 16)) (y : (BitVec 16)) : (BitVec 16) :=
  (HXor.hXor x y)

def bitvector_unsigned (x : (BitVec 16)) : Nat :=
  (BitVec.toNat x)

def bitvector_signed (x : (BitVec 16)) : Int :=
  (BitVec.toInt x)

def initialize_registers : Unit :=
  ()

