def List.zipWithState {α β γ δ: Type} (f : α → β → γ → α × δ) : α → List β → List γ → List δ
| _, nil, _ => nil
| _, _, nil => nil
| state, (cons x xs), (cons y ys) => let (new_state, new_z) := f state x y
                                     let final_zs := zipWithState f new_state xs ys
                                     cons new_z final_zs

theorem zipWithState_same_length : ∀ {α β γ δ: Type} (f : α → β → γ → α × δ) (state : α) (xs : List β) (ys : List γ), xs.length = ys.length → (List.zipWithState f state xs ys).length = xs.length := by
  intro α β γ δ f state xs ys
  induction xs generalizing ys
  case nil => intro h; simp [List.zipWithState]
  case cons x xs ih => intro h
                       cases ys with
                       | nil => rw [h]; simp [List.zipWithState]
                       | cons y ys => simp [List.zipWithState]
                                      rw [← List.length_cons x xs, ← List.length_cons y ys]
                                      simp [List.zipWithState]
                                      exact ih (Nat.succ.inj h)

theorem Nat.le_zero_iff : n ≤ 0 ↔ n = 0 := by
  cases n with
  | zero => simp
  | succ n => simp

inductive Bit where
  | zero
  | one
  deriving BEq, Repr

inductive Decimal where
  | zero | one | two | three | four | five | six | seven | eight | nine

inductive ComparisonResult
  | greaterThan | lesserThan | equal | unordered

structure BitField (len : Nat): Type where
  repr : List Bit
  repr_has_len : len = repr.length

namespace BitField

def allOnes (w : Nat) : BitField w :=
  ⟨List.replicate w Bit.one, by simp⟩

def isAllOnes (w : Nat) (vec : BitField w) : Bool :=
  vec.repr.all (λ x => x == Bit.one)

def allZeroes (w : Nat) : BitField w :=
  ⟨List.replicate w Bit.zero, by simp⟩

def isAllZeroes (w : Nat) (vec : BitField w) : Bool :=
  vec.repr.all (λ x => x == Bit.zero)

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
          ⟨(List.zipWithState adder Bit.zero (a.repr.reverse) (b.repr.reverse)).reverse,
            by simp [repr_has_len]⟩

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
          List.reverse (Vec.zipWithState subtractor Bit.zero (reverse a) (reverse b))

×

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
