import Out.Sail.Sail

open Sail

class MonadReg where
  set_R0: BitVec 64 -> SailM Unit
  get_R0: SailM (BitVec 64)

variable [MonadReg]

open MonadReg

def initialize_registers : SailM Unit := do
  let w__0 := (undefined_bitvector 64)
  set_R0 w__0
  return ()
