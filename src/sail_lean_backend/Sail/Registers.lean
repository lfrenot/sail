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
