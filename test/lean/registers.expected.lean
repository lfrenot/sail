import Out.Sail.Sail

open Sail

inductive register_bit where
  | BIT
deriving DecidableEq
open register_bit

inductive register_bitvector_64 where
  | R0
  | R1
deriving DecidableEq
open register_bitvector_64

inductive register_bool where
  | BOOL
deriving DecidableEq
open register_bool

inductive register_int where
  | INT
deriving DecidableEq
open register_int

inductive register_nat where
  | NAT
deriving DecidableEq
open register_nat


inductive Register : Type -> Type where
  | R_bit : register_bit -> Register (BitVec 1)
  | R_bitvector_64 : register_bitvector_64 -> Register (BitVec 64)
  | R_bool : register_bool -> Register Bool
  | R_int : register_int -> Register Int
  | R_nat : register_nat -> Register Nat

instance : Coe register_bit (Register (BitVec 1)) where
  coe r := Register.R_bit r
instance : Coe register_bitvector_64 (Register (BitVec 64)) where
  coe r := Register.R_bitvector_64 r
instance : Coe register_bool (Register Bool) where
  coe r := Register.R_bool r
instance : Coe register_int (Register Int) where
  coe r := Register.R_int r
instance : Coe register_nat (Register Nat) where
  coe r := Register.R_nat r

structure Regstate where
  bit_s : register_bit -> (BitVec 1)
  bitvector_64_s : register_bitvector_64 -> (BitVec 64)
  bool_s : register_bool -> Bool
  int_s : register_int -> Int
  nat_s : register_nat -> Nat


def register_lookup {T : Type} (reg : Register T) (rs : Regstate) : T :=
  match reg with
  | Register.R_bit r => rs.bit_s r
  | Register.R_bitvector_64 r => rs.bitvector_64_s r
  | Register.R_bool r => rs.bool_s r
  | Register.R_int r => rs.int_s r
  | Register.R_nat r => rs.nat_s r

def register_set {T : Type} (reg : Register T) : T -> Regstate -> Regstate :=
  match reg with
  | Register.R_bit r => fun v rs => { rs with bit_s := fun r' => if r' = r then v else rs.bit_s r' }
  | Register.R_bitvector_64 r => fun v rs => { rs with bitvector_64_s := fun r' => if r' = r then v else rs.bitvector_64_s r' }
  | Register.R_bool r => fun v rs => { rs with bool_s := fun r' => if r' = r then v else rs.bool_s r' }
  | Register.R_int r => fun v rs => { rs with int_s := fun r' => if r' = r then v else rs.int_s r' }
  | Register.R_nat r => fun v rs => { rs with nat_s := fun r' => if r' = r then v else rs.nat_s r' }


abbrev SailM := PreSailM Regstate
def read_reg {T : Type} : Register T -> SailM T := @Sail.read_reg _ T _ @register_lookup
def write_reg {T : Type} : Register T -> T -> SailM Unit := @Sail.write_reg _ T _ @register_set
def reg_deref {T : Type} : RegisterRef Register T → SailM T := @Sail.reg_deref _ T _ @read_reg

def initialize_registers : SailM Unit := do
  write_reg R0 sorry
  write_reg R1 sorry
  write_reg INT sorry
  write_reg BOOL sorry
  write_reg NAT sorry
  write_reg BIT sorry

