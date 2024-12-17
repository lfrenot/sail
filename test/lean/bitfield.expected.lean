import Sail.Sail

structure cr_type  where
  bits : BitVec 8


def undefined_cr_type (lit : Unit) : cr_type :=
  sorry

def Mk_cr_type (v : BitVec 8) : cr_type :=
  {bits := v}

def _get_cr_type_bits (v : cr_type) : BitVec 8 :=
  (Sail.BitVec.extractLsb v.bits (HSub.hSub 8 1) 0)

def _update_cr_type_bits (v : cr_type) (x : BitVec 8) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits (HSub.hSub 8 1) 0 x)}

def _set_cr_type_bits (r_ref : register_ref Unit Unit cr_type) (v : BitVec 8) : Unit :=
  sorry

def _get_cr_type_CR0 (v : cr_type) : BitVec 4 :=
  (Sail.BitVec.extractLsb v.bits 7 4)

def _update_cr_type_CR0 (v : cr_type) (x : BitVec 4) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits 7 4 x)}

def _set_cr_type_CR0 (r_ref : register_ref Unit Unit cr_type) (v : BitVec 4) : Unit :=
  sorry

def _get_cr_type_CR1 (v : cr_type) : BitVec 2 :=
  (Sail.BitVec.extractLsb v.bits 3 2)

def _update_cr_type_CR1 (v : cr_type) (x : BitVec 2) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits 3 2 x)}

def _set_cr_type_CR1 (r_ref : register_ref Unit Unit cr_type) (v : BitVec 2) : Unit :=
  sorry

def _get_cr_type_CR3 (v : cr_type) : BitVec 2 :=
  (Sail.BitVec.extractLsb v.bits 1 0)

def _update_cr_type_CR3 (v : cr_type) (x : BitVec 2) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits 1 0 x)}

def _set_cr_type_CR3 (r_ref : register_ref Unit Unit cr_type) (v : BitVec 2) : Unit :=
  sorry

def _get_cr_type_GT (v : cr_type) : BitVec 1 :=
  (Sail.BitVec.extractLsb v.bits 6 6)

def _update_cr_type_GT (v : cr_type) (x : BitVec 1) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits 6 6 x)}

def _set_cr_type_GT (r_ref : register_ref Unit Unit cr_type) (v : BitVec 1) : Unit :=
  sorry

def _get_cr_type_LT (v : cr_type) : BitVec 1 :=
  (Sail.BitVec.extractLsb v.bits 7 7)

def _update_cr_type_LT (v : cr_type) (x : BitVec 1) : cr_type :=
  {v with bits := (Sail.BitVec.update_subrange v.bits 7 7 x)}

def _set_cr_type_LT (r_ref : register_ref Unit Unit cr_type) (v : BitVec 1) : Unit :=
  sorry

def initialize_registers : Unit :=
  ()

