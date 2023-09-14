inductive Vec (α : Type u) : Nat → Type u
  | nil  : Vec α 0
  | cons : α → Vec α n → Vec α (n+1)

namespace Vec

def first {n : Nat} : (Vec α n) -> Option α
| nil => none
| cons x _ => some x

def last {α : Type u} {n : Nat} : (Vec α n) → α
| cons x₁ xs₁  => match xs₁ with
                | nil => x₁
                | cons x₂ xs₂ => last (cons x₂ xs₂)

def reverse {α : Type} {n : Nat} : (Vec α n) → Vec α n
  | nil => nil

def zipWith {α β γ : Type} {n : Nat} (f : α → β → γ) : (Vec α n) -> (Vec β n) -> (Vec γ n)
  | nil, nil => nil
  | (cons x xs), (cons y ys) => (cons (f x y) (zipWith f xs ys))

def map {α β: Type} {n : Nat} (f : α → β) : (Vec α n) → (Vec β n)
| nil => nil
| (cons x xs) => (cons (f x) (map f xs))

def foldr {α β : Type} {n : Nat} (f : α → β → β) (state : β) : (Vec α n) → β
| nil => state
| cons x xs =>  (f x (foldr f state xs))

def zipFold {α β γ : Type} {n : Nat} (f : α → β → γ → γ) (state : γ) : (Vec α n) → (Vec β n) → γ
| nil, nil => state
| (cons x xs), (cons y ys) => (f x y (zipFold f state xs ys))

def all {α : Type} (vec : Vec α n) (f : α → Bool) : Bool :=
    match vec with
    | nil => true
    | cons x xs => f x && all xs f

def replicate {α : Type} (val : α) (n : Nat) : Vec α n:=
  match n with
  | 0 => nil
  | n + 1 => cons val (Vec.replicate val n)

def concat {n m : Nat} (xs : Vec α n) (ys : Vec α m) : Vec α (m + n) :=
  match n, m, xs, ys with
  | 0, _, nil, ys => ys
  | (Nat.succ _), _, cons x xs, ys => cons x (concat xs ys)

end Vec
open Vec

inductive Bit where
  | zero
  | one
  deriving BEq, Repr

inductive Decimal where
  | zero | one | two | three | four | five | six | seven | eight | nine

def BitField : (Nat → Type) := Vec Bit

def add : (BitField n) → (BitField n) → (BitField n) :=
  Vec.zipWith (λ x y => match x, y with
                        | Bit.zero, Bit.zero => Bit.zero
                        | Bit.zero, Bit.one => Bit.one
                        | Bit.one, Bit.zero => Bit.one
                        | Bit.one, Bit.one => Bit.zero)

def allOnes (w : Nat) : BitField w :=
  Vec.replicate Bit.one w

def allZeroes (w : Nat) : BitField w :=
  Vec.replicate Bit.zero w

inductive Error where
  | invalid_operation

structure IEEEFloat (exponent_len significand_len : { a : Nat // ( a ≥ 1 )}) where
  sign : Bit
  exponent : BitField exponent_len
  significand : BitField significand_len

namespace IEEEFloat

def IsNaN (n : IEEEFloat exponent_len significant_len) : Bool :=
    n.exponent.all (λ x => x == Bit.one) && not (n.significand.all (λ x => x == Bit.zero))

def IsInf (n : IEEEFloat w t ) : Bool :=
    n.exponent.all (λ x => x == Bit.one) && n.significand.all (λ x => x == Bit.zero)

def IsPlusInf (n : IEEEFloat w t ) : Bool :=
    n.sign == Bit.zero && IsInf n

def IsMinusInf (n : IEEEFloat w t ) : Bool :=
    n.sign == Bit.one && IsInf n

def IsZero (n : IEEEFloat w t ) : Bool :=
    n.exponent.all (λ x => x == Bit.zero) && n.significand.all (λ x => x == Bit.zero)

def IsPlusZero (n : IEEEFloat w t ) : Bool :=
    n.sign == Bit.zero && IsZero n

def IsMinusZero (n : IEEEFloat w t ) : Bool :=
    n.sign == Bit.one && IsZero n

-- def roundToIntegralTiesToEven (n : IEEEFloat w t ) : n :=
--   if n.IsNaN or n.IsInf then
--     n
--   else

theorem concatted_vec_is_bitfield (vec : Vec Bit (orig_len - 1 + (0 + 1))) {gt_zero_len : 1 ≤ orig_len}: BitField orig_len := by
  simp at vec;
  rw [Nat.sub_add_cancel] at vec;
  exact vec;
  exact gt_zero_len

def nextUp (n : IEEEFloat w t) : IEEEFloat w t :=
  if n.IsNaN || n.IsPlusInf then
    n
  else if n.IsMinusInf then
    { sign := Bit.one,
      exponent := allOnes w,
      significand := allOnes t}
  else if n.IsZero then
    { sign := Bit.zero,
      exponent := allZeroes w,
      significand := concatted_vec_is_bitfield (concat (replicate Bit.zero (t - 1)) (cons Bit.one nil)) }
  else if n.significand.all (· == Bit.one) then
    { sign := n.sign,
      exponent := concatted_vec_is_bitfield (concat (replicate Bit.zero (w - 1)) (cons Bit.one nil)),
      significand := allZeroes t}
  else
    { sign := n.sign,
      exponent := n.exponent,
      significand := n.significand.cons Bit.one}

end IEEEFloat
open IEEEFloat

def binary16 : Type := IEEEFloat ⟨5, by decide⟩ 10
def binary32 : Type := IEEEFloat 8 23
def binary64 : Type := IEEEFloat 11 52
def binary128 : Type := IEEEFloat 15 112
