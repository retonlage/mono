# The rational numbers

```agda
module elementary-number-theory.rational-numbers where
```

<details><summary>Imports</summary>

```agda
open import elementary-number-theory.divisibility-integers
open import elementary-number-theory.greatest-common-divisor-integers
open import elementary-number-theory.integer-fractions
open import elementary-number-theory.integers
open import elementary-number-theory.reduced-integer-fractions

open import foundation.dependent-pair-types
open import foundation.equality-cartesian-product-types
open import foundation.equality-dependent-pair-types
open import foundation.identity-types
open import foundation.negation
open import foundation.propositions
open import foundation.reflecting-maps-equivalence-relations
open import foundation.sets
open import foundation.universe-levels
```

</details>

## Idea

The type of rational numbers is the quotient of the type of fractions, by the
equivalence relation given by `(n/m) ~ (n'/m') := Id (n *ℤ m') (n' *ℤ m)`.

## Definitions

### The type of rationals

```agda
ℚ : UU lzero
ℚ = Σ fraction-ℤ is-reduced-fraction-ℤ

fraction-ℚ : ℚ → fraction-ℤ
fraction-ℚ x = pr1 x

is-reduced-fraction-ℚ : (x : ℚ) → is-reduced-fraction-ℤ (fraction-ℚ x)
is-reduced-fraction-ℚ x = pr2 x
```

### Inclusion of fractions

```agda
in-fraction-ℤ : fraction-ℤ → ℚ
pr1 (in-fraction-ℤ x) = reduce-fraction-ℤ x
pr2 (in-fraction-ℤ x) = is-reduced-reduce-fraction-ℤ x
```

### Inclusion of the integers

```agda
in-int : ℤ → ℚ
in-int x = pair (pair x one-positive-ℤ) (is-one-gcd-one-ℤ' x)
```

### Negative one, zero and one

```agda
neg-one-ℚ : ℚ
neg-one-ℚ = in-int neg-one-ℤ

is-neg-one-ℚ : ℚ → UU lzero
is-neg-one-ℚ x = (x ＝ neg-one-ℚ)

zero-ℚ : ℚ
zero-ℚ = in-int zero-ℤ

is-zero-ℚ : ℚ → UU lzero
is-zero-ℚ x = (x ＝ zero-ℚ)

is-nonzero-ℚ : ℚ → UU lzero
is-nonzero-ℚ k = ¬ (is-zero-ℚ k)

one-ℚ : ℚ
one-ℚ = in-int one-ℤ

is-one-ℚ : ℚ → UU lzero
is-one-ℚ x = (x ＝ one-ℚ)
```

## Properties

### If two fractions are related by `sim-fraction-ℤ`, then their embeddings into `ℚ` are equal

```agda
eq-ℚ-sim-fractions-ℤ :
  (x y : fraction-ℤ) → (H : sim-fraction-ℤ x y) →
  in-fraction-ℤ x ＝ in-fraction-ℤ y
eq-ℚ-sim-fractions-ℤ x y H =
  eq-pair-Σ'
    ( pair
      ( unique-reduce-fraction-ℤ x y H)
      ( eq-is-prop (is-prop-is-reduced-fraction-ℤ (reduce-fraction-ℤ y))))
```

### The type of rationals is a set

```agda
is-set-ℚ : is-set ℚ
is-set-ℚ =
  is-set-Σ
    ( is-set-fraction-ℤ)
    ( λ x → is-set-is-prop (is-prop-is-reduced-fraction-ℤ x))

ℚ-Set : Set lzero
pr1 ℚ-Set = ℚ
pr2 ℚ-Set = is-set-ℚ

in-fraction-fraction-ℚ : (x : ℚ) → in-fraction-ℤ (fraction-ℚ x) ＝ x
in-fraction-fraction-ℚ (pair (pair m (pair n n-pos)) p) =
  eq-pair-Σ
    ( eq-pair
      ( eq-quotient-div-is-one-ℤ _ _ p (div-left-gcd-ℤ m n))
      ( eq-pair-Σ
        ( eq-quotient-div-is-one-ℤ _ _ p (div-right-gcd-ℤ m n))
        ( eq-is-prop (is-prop-is-positive-ℤ n))))
    ( eq-is-prop (is-prop-is-reduced-fraction-ℤ (m , n , n-pos)))
```

### The reflecting map from ℤ to ℚ

```agda
reflecting-map-sim-fraction :
  reflecting-map-Equivalence-Relation eq-rel-sim-fraction-ℤ ℚ
pr1 reflecting-map-sim-fraction = in-fraction-ℤ
pr2 reflecting-map-sim-fraction {x} {y} H = eq-ℚ-sim-fractions-ℤ x y H
```
