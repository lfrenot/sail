default Order dec

$include <prelude.sail>

struct My_struct = {
  field1 : int,
  field2 : bit,
}

val struct_field2 : My_struct -> bit
function struct_field2(s) = {
  s.field2
}

val struct_update_field2 : (My_struct, bit) -> My_struct
function struct_update_field2(s, b) = {
  { s with field2 = b }
}