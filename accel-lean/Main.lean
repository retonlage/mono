inductive Error where
    | stack_underflow
    | invalid_jump

structure VMState where
  pc : Nat
  stack : List Nat

def initVMState : VMState :=
  { pc := 0, stack := [] }

inductive Instruction where
    | push (n : Nat)
    | add
    | sub
    | mul
    | eq
    | jmp (n : Nat)
    | jz (n : Nat)
    | jnz (n : Nat)

def pop_underflow (n : Nat) : ExceptT Error (StateM VMState) (List Nat) := do
    vmState <- get
    if vmState.stack.length < n then
        throw Error.stack_underflow
    else
        let (a, b) := vmState.stack.splitAt n
        modifyGet λ vmState => { vmState with stack := b }
        return a

def binop (f : Nat → Nat → Nat) : ExceptT Error (StateM VMState) Unit := do
  let [a, b] = (← pop_underflow 2)
  modifyGet λ vmState => { vmState with stack := vmState.stack.drop 2 }
  modifyGet λ vmState => { vmState with stack := (f a b) :: vmState.stack }

def run_single_instruction (instr : Instruction) : ExceptT Error (StateM VMState) Unit := do
    vmState <- get
    let newState = match instr with
    | Instruction.push n =>
        modifyGet λ vmState => { vmState with stack := n :: vmState.stack }
    | Instruction.add => do
        binop (· + ·)
    | Instruction.sub => do
        binop (· - ·)
    | Instruction.mul => do
        binop (· * ·)
    | Instruction.eq => do
        binop (λ a b => if a == b then 1 else 0)
    | Instruction.jmp n => do
        vmState.modifyGet λ vmState => { vmState with pc := n }
    | Instruction.jz n => do
        let [a] := take_underflow
        if a == 0 then
        vmState with pc := n
    | Instruction.jnz n => do
        let [a] := take_underflow
        if a != 0 then
        vmState with pc := n
    return newState

-- def vm (program : List Instruction)  :=


def main : IO Unit :=
  IO.println "Hello, world!"
