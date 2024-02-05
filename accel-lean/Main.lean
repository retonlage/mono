structure Label where
  name : String

inductive Identifier where
| named (name : String)
| unnamed (index : Nat)

structure Value where
  is_local : Bool
  identifier: Identifier

inductive Type where
| int (width : Nat)
| half | float | double | fp128

inductive Terminator where
| ret | retval (type : Type) (value : Value)
| br (cond : Value) (then_label : Label) (else_label : Label)
| switch (cond : Value) (default_label : Label) (cases : List (Value × Label))
| invoke

inductive Instruction where
  |
