
inductive register where
  | Register : String → /- name -/
               Nat → /- length -/
               Nat → /- start index -/
               Bool → /- is increasing -/
               List register_field_index
               → register
  | UndefinedRegister : Nat → register /- length -/
  | RegisterPair : register → register → register

structure register_ref (regstate regval a: Type) where
  name: String
  read_from: regstate -> a
  write_to: a -> regstate -> regstate
  of_regval: regval -> Option a
  regval_of: a -> regval

def read_reg {s rv a e} (reg : register_ref s rv a) : Monad e :=
  let k v :=
    match reg.of_regval v with
      | some v => some v
      | none => none
  Read_reg reg.(name) k.

def reg_deref {s rv a e} := @read_reg s rv a e.

def write_reg {s rv a e} (reg : register_ref s rv a) (v : a) : monad rv unit e :=
 Write_reg reg.(name) (reg.(regval_of) v) (Done tt).
