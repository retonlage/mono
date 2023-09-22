inductive Vec (α : Type u) : Nat → Type u
  | nil  : Vec α 0
  | cons : α → Vec α n → Vec α (n + 1)
  deriving Repr

namespace Vec

def concat (xs : Vec α n) (ys : Vec α m) : Vec α (m + n) :=
  match n, m, xs, ys with
  | 0, _, nil, ys => ys
  | (Nat.succ _), _, cons x xs, ys => cons x (concat xs ys)

theorem vec_len_n_plus_1_comm : Vec α (1 + n) = Vec α (n + 1) := by
  rw [Nat.add_comm]

def snoc (xs : Vec α n) (x : α) : Vec α (n + 1) :=
  vec_len_n_plus_1_comm ▸ (concat xs (cons x nil))

def vector_is_not_nil (vec : Vec α n) : Prop :=
  match vec with
  | nil => False
  | cons _ _ => True

def head_option : Vec α n -> Option α
| nil => none
| cons x _ => some x

def head : Vec α (n + 1) → α
| cons x _ => x

def last : Vec α (n + 1) → α
| cons x xs => match xs with
               | nil => x
               | cons y ys => last (cons y ys)

def body : Vec α (n + 1) → (Vec α n)
| cons x xs => match xs with
               | nil => nil
               | cons y ys => cons x (body (cons y ys))

def reverse : Vec α n → Vec α n
| nil => nil
| cons x xs => snoc (reverse xs) x

def zipWith {n : Nat} (f : α → β → γ) : (Vec α n) -> (Vec β n) -> (Vec γ n)
  | nil, nil => nil
  | (cons x xs), (cons y ys) => (cons (f x y) (zipWith f xs ys))

def map (f : α → β) : (Vec α n) → (Vec β n)
| nil => nil
| (cons x xs) => (cons (f x) (map f xs))

def foldr {n : Nat} (f : α → β → β) (state : β) : (Vec α n) → β
| nil => state
| cons x xs =>  (f x (foldr f state xs))

def zipFold {n : Nat} (f : α → β → γ → γ) (state : γ) : (Vec α n) → (Vec β n) → γ
| nil, nil => state
| (cons x xs), (cons y ys) => (f x y (zipFold f state xs ys))

-- TODO i am not greek use actual names
def zipWithState (f : α → β → γ → δ × γ) (state : γ) : (Vec α n) → (Vec β n) → (Vec δ n)
| nil, nil => nil
| (cons x xs), (cons y ys) => let (z, new_state) := (f x y state);
                              (cons z (zipWithState f new_state xs ys))

mutual
  def take {n_is_lt_orig_len : n ≤ orig_len} (n : Nat) (vec : Vec α orig_len) : (Vec α n) :=
  match n with
  | 0 => nil
  | n + 1 => take_cons ⟨n+1, by ⟩ vec

  def take_cons : {a : Nat // a ≥ 1} → (Vec α (len + 1)) → (Vec α n)
  | n + 1, cons x xs => cons x (take n xs)
end

def all (vec : Vec α n) (f : α → Bool) : Bool :=
    match vec with
    | nil => true
    | cons x xs => f x && all xs f

def replicate (val : α) (n : Nat) : Vec α n:=
  match n with
  | 0 => nil
  | n + 1 => cons val (Vec.replicate val n)

end Vec
open Vec

inductive Bit where
  | zero
  | one
  deriving BEq, Repr

inductive Decimal where
  | zero | one | two | three | four | five | six | seven | eight | nine

inductive ComparisonResult
  | greaterThan | lesserThan | equal | unordered

def BitField : (Nat → Type) := Vec Bit

namespace BitField

-- TODO: rewrite as proper equality theorem
theorem concatted_vec_is_bitfield (vec : Vec Bit (orig_len - 1 + (0 + 1))) {gt_zero_len : 1 ≤ orig_len}: BitField orig_len := by
  simp at vec;
  rw [Nat.sub_add_cancel] at vec;
  exact vec;
  exact gt_zero_len

def allOnes (w : Nat) : BitField w :=
  Vec.replicate Bit.one w

def isAllOnes (w : Nat) (vec : BitField w) : Bool :=
  vec.all (λ x => x == Bit.one)

def allZeroes (w : Nat) : BitField w :=
  Vec.replicate Bit.zero w

def isAllZeroes (w : Nat) (vec : BitField w) : Bool :=
  vec.all (λ x => x == Bit.zero)

def saturating_add : (BitField n) → (BitField n) → (BitField n)
| a, b => let adder := (λ x y carry => match x, y, carry with
                        | Bit.zero, Bit.zero, Bit.zero => (Bit.zero, Bit.zero)
                        | Bit.zero, Bit.zero, Bit.one  => (Bit.one , Bit.zero)
                        | Bit.zero, Bit.one , Bit.zero => (Bit.one , Bit.zero)
                        | Bit.one , Bit.zero, Bit.zero => (Bit.one , Bit.zero)
                        | Bit.zero, Bit.one , Bit.one  => (Bit.zero, Bit.one)
                        | Bit.one , Bit.zero, Bit.one  => (Bit.zero, Bit.one)
                        | Bit.one , Bit.one , Bit.zero => (Bit.zero, Bit.one)
                        | Bit.one , Bit.one , Bit.one  => (Bit.one , Bit.one));
          reverse (Vec.zipWithState adder Bit.zero (reverse a) (reverse b))

def saturating_subtract : (BitField n) → (BitField n) → (BitField n)
| a, b => let subtractor := (λ x y carry => match x, y, carry with
                        | Bit.zero, Bit.zero, Bit.zero => (Bit.zero, Bit.zero)
                        | Bit.zero, Bit.zero, Bit.one  => (Bit.one , Bit.one)
                        | Bit.zero, Bit.one , Bit.zero => (Bit.one , Bit.one)
                        | Bit.one , Bit.zero, Bit.zero => (Bit.one , Bit.zero)
                        | Bit.zero, Bit.one , Bit.one  => (Bit.zero, Bit.one)
                        | Bit.one , Bit.zero, Bit.one  => (Bit.zero, Bit.zero)
                        | Bit.one , Bit.one , Bit.zero => (Bit.zero, Bit.zero)
                        | Bit.one , Bit.one , Bit.one  => (Bit.zero, Bit.one));
          reverse (Vec.zipWithState subtractor Bit.zero (reverse a) (reverse b))

theorem tt (n_gt_0: 1 ≤ n) : BitField (n - 1 + 1) = BitField n := by
  rw [Nat.sub_add_cancel]
  exact n_gt_0

def saturating_incr {as : 1 ≤ n} : (BitField n) → (BitField n)
| bit_field => bit_field.saturating_add (tt as ▸ (snoc (allZeroes (n - 1)) Bit.one))

def saturating_decr {as : 1 ≤ n} : (BitField n) → (BitField n)
| bit_field => bit_field.saturating_subtract (tt as ▸ (snoc (allZeroes (n - 1)) Bit.one))

def bitcompare : (BitField len) → (BitField len) → ComparisonResult
  | nil, nil => ComparisonResult.equal
  | (Vec.cons Bit.one _ ), (cons Bit.zero _) => ComparisonResult.greaterThan
  | (Vec.cons Bit.zero _), (cons Bit.one _ ) => ComparisonResult.lesserThan
  | (Vec.cons _ xs), (cons _ ys ) => bitcompare xs ys

end BitField
open BitField

inductive Error where
  | invalid_operation

structure IEEEFloat (exponent_len significand_len : { a : Nat // ( a ≥ 1 )}) where
  sign : Bit
  exponent : BitField exponent_len
  significand : BitField significand_len

namespace IEEEFloat

def asUnsignedBitField : (IEEEFloat exponent_len significand_len) → BitField (significand_len + exponent_len)
| n => (concat n.exponent n.significand)

def fromUnsignedBitField {significand_len exponent_len : { a : Nat // ( a ≥ 1 )}} : Bit → (BitField (significand_len + exponent_len)) → IEEEFloat exponent_len significand_len
| sign, bits => ⟨sign, bits.take exponent_len, bits.drop exponent_len⟩

def asBitField : (IEEEFloat exponent_len significand_len) → (BitField (significand_len + exponent_len + 1))
| n => cons n.sign n.asUnsignedBitField

def isNaN (n : IEEEFloat exponent_len significand_len) : Bool :=
  n.exponent.all (λ x => x == Bit.one) && not (n.significand.all (λ x => x == Bit.zero))

-- def isQuiet {n_is_nan : n.isNaN = true } (n : IEEEFloat exponent_len significant_len) : Bool :=
--   n.

def isInf (n : IEEEFloat w t ) : Bool :=
  n.exponent.all (λ x => x == Bit.one) && n.significand.all (λ x => x == Bit.zero)

def isPlusInf (n : IEEEFloat w t ) : Bool :=
  n.sign == Bit.zero && isInf n

def isMinusInf (n : IEEEFloat w t ) : Bool :=
  n.sign == Bit.one && isInf n

def isZero (n : IEEEFloat w t ) : Bool :=
  n.exponent.all (λ x => x == Bit.zero) && n.significand.all (λ x => x == Bit.zero)

def isPlusZero (n : IEEEFloat w t ) : Bool :=
  n.sign == Bit.zero && isZero n

def isMinusZero (n : IEEEFloat w t ) : Bool :=
  n.sign == Bit.one && isZero n

def isSubNormal (n : IEEEFloat w t ) : Bool :=
  n.exponent.all (λ x => x == Bit.zero) && not (n.significand.all (λ x => x == Bit.zero))

def plusInf : IEEEFloat w t :=
  { sign := Bit.zero,
    exponent := allOnes w,
    significand := allZeroes t}

def minusInf : IEEEFloat w t :=
  { sign := Bit.one,
    exponent := allOnes w,
    significand := allZeroes t}

def plusZero : IEEEFloat w t :=
  { sign := Bit.zero,
    exponent := allZeroes w,
    significand := allZeroes t}

def minusZero : IEEEFloat w t :=
  { sign := Bit.one,
    exponent := allZeroes w,
    significand := allZeroes t}

def compare (n m : IEEEFloat w t) : ComparisonResult :=
  if n.isNaN  || m.isNaN then
    ComparisonResult.unordered
  else if n.isZero && m.isZero then -- -0 == 0
    ComparisonResult.equal
  else if n.sign == Bit.one && m.sign == Bit.zero then
    ComparisonResult.lesserThan
  else if n.sign == Bit.zero && m.sign == Bit.one then
    ComparisonResult.greaterThan
  else
    bitcompare (n.asUnsignedBitField) (m.asUnsignedBitField)

-- def roundToIntegralTiesToEven (n : IEEEFloat w t ) : n :=
--   if n.IsNaN or n.IsInf then
--     n
--   else

def nextUp (n : IEEEFloat w t) : IEEEFloat w t :=
  if n.isNaN || n.isPlusInf then
    n
  else if n.isMinusInf then
    { sign := Bit.one,
      exponent := concatted_vec_is_bitfield (cons Bit.zero (allOnes (w - 1))),
      significand := allOnes t }
  else if n.isZero then
    { sign := Bit.zero,
      exponent := concatted_vec_is_bitfield (snoc (replicate Bit.zero (w - 1)) Bit.one),
      significand := concatted_vec_is_bitfield (snoc (replicate Bit.zero (t - 1)) Bit.one) }
  else if n.sign == Bit.zero then
    if ! n.significand.isAllOnes then
      { sign := n.sign,
        exponent := n.exponent,
        significand := n.significand.saturating_incr}
    else
      { sign := n.sign,
        exponent := (n.exponent.saturating_incr),
        significand := allZeroes t}
  else
    { sign := n.sign,
      exponent := n.exponent,
      significand := n.significand.cons}

end IEEEFloat
open IEEEFloat

-- def binary16 : Type := IEEEFloat ⟨5, by decide⟩ 10
-- def binary32 : Type := IEEEFloat 8 23
-- def binary64 : Type := IEEEFloat 11 52
-- def binary128 : Type := IEEEFloat 15 112
