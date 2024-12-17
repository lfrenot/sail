import Out.Sail.Sail

def xlen : Int := 64

def xlen_bytes : Int := 8

def xlenbits := BitVec 64

def EXTZ {m : _} (v : BitVec k_n) : BitVec m :=
  (Sail.BitVec.zeroExtend v m)

def EXTS {m : _} (v : BitVec k_n) : BitVec m :=
  (Sail.BitVec.signExtend v m)

def initialize_registers : Unit :=
  ()
