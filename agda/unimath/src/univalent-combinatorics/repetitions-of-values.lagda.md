# Repetitions of values

```agda
module univalent-combinatorics.repetitions-of-values where

open import foundation.repetitions-of-values public
```

<details><summary>Imports</summary>

```agda
open import elementary-number-theory.natural-numbers
open import elementary-number-theory.well-ordering-principle-standard-finite-types

open import foundation.cartesian-product-types
open import foundation.decidable-types
open import foundation.identity-types
open import foundation.injective-maps
open import foundation.negation

open import univalent-combinatorics.decidable-dependent-function-types
open import univalent-combinatorics.decidable-propositions
open import univalent-combinatorics.dependent-pair-types
open import univalent-combinatorics.equality-standard-finite-types
open import univalent-combinatorics.standard-finite-types
```

</details>

## Idea

A **repetition of values** of a function `f : A → B` consists of a pair
`a a' : A` such that `a ≠ a'` and `f a ＝ f a'`.

## Properties

### If `f : Fin k → Fin l` is not injective, then it has a repetition of values

b

```agda
repetition-of-values-is-not-injective-Fin :
  (k l : ℕ) (f : Fin k → Fin l) →
  is-not-injective f → repetition-of-values f
repetition-of-values-is-not-injective-Fin k l f N =
  pair (pair x (pair y (pr2 w))) (pr1 w)
  where
  u : Σ (Fin k) (λ x → ¬ ((y : Fin k) → f x ＝ f y → x ＝ y))
  u =
    exists-not-not-forall-Fin k
      ( λ x →
        is-decidable-Π-Fin k
          ( λ y →
            is-decidable-function-type
              ( has-decidable-equality-Fin l (f x) (f y))
              ( has-decidable-equality-Fin k x y)))
      ( λ f → N (λ {x} {y} → f x y))
  x : Fin k
  x = pr1 u
  H : ¬ ((y : Fin k) → Id (f x) (f y) → Id x y)
  H = pr2 u
  v : Σ (Fin k) (λ y → ¬ (Id (f x) (f y) → Id x y))
  v = exists-not-not-forall-Fin k
      ( λ y →
        is-decidable-function-type
          ( has-decidable-equality-Fin l (f x) (f y))
          ( has-decidable-equality-Fin k x y))
      ( H)
  y : Fin k
  y = pr1 v
  K : ¬ (Id (f x) (f y) → Id x y)
  K = pr2 v
  w : Id (f x) (f y) × ¬ (Id x y)
  w = exists-not-not-forall-count
      ( λ _ → Id x y)
      ( λ _ →
        has-decidable-equality-Fin k x y)
      ( count-is-decidable-is-prop
        ( is-set-Fin l (f x) (f y))
        ( has-decidable-equality-Fin l (f x) (f y)))
      ( K)
```

### On the standard finite sets, `is-repetition-of-values f x` is decidable

```agda
-- is-decidable-is-repetition-of-values-Fin :
--   {k l : ℕ} (f : Fin k → Fin l) (x : Fin k) →
--   is-decidable (is-repetition-of-values f x)
-- is-decidable-is-repetition-of-values-Fin f x =
--   is-decidable-Σ-Fin
--     ( λ y →
--       is-decidable-prod
--         ( is-decidable-neg (has-decidable-equality-Fin x y))
--         ( has-decidable-equality-Fin (f x) (f y)))
```

### On the standard finite sets, `is-repeated-value f x` is decidable

```agda
-- is-decidable-is-repeated-value-Fin :
--   (k l : ℕ) (f : Fin k → Fin l) (x : Fin k) →
--   is-decidable (is-repeated-value f x)
-- is-decidable-is-repeated-value-Fin k l f x =
--   is-decidable-Σ-Fin k
--     ( λ y →
--       is-decidable-prod
--         ( is-decidable-neg (has-decidable-equality-Fin k x y))
--         ( has-decidable-equality-Fin l (f x) (f y)))
```

### The predicate that `f` maps two different elements to the same value

This remains to be defined.

### On the standard finite sets, `has-repetition-of-values f` is decidable

```agda
-- is-decidable-has-repetition-of-values-Fin :
--   (k l : ℕ) (f : Fin k → Fin l) → is-decidable (has-repetition-of-values f)
-- is-decidable-has-repetition-of-values-Fin k l f =
--   is-decidable-Σ-Fin k (is-decidable-is-repetition-of-values-Fin k l f)
```

### If `f` is not injective, then it has a `repetition-of-values`

```agda
is-injective-map-Fin-zero-Fin :
  {k : ℕ} (f : Fin zero-ℕ → Fin k) → is-injective f
is-injective-map-Fin-zero-Fin f {()} {y}

-- is-injective-map-Fin-one-Fin : {k : ℕ} (f : Fin 1 → Fin k) → is-injective f
-- is-injective-map-Fin-one-Fin f {inr star} {inr star} p = refl

-- has-repetition-of-values-is-not-injective-Fin :
--   (k l : ℕ) (f : Fin l → Fin k) →
--   is-not-injective f → has-repetition-of-values f
-- has-repetition-of-values-is-not-injective-Fin k zero-ℕ f H =
--   ex-falso (H (is-injective-map-Fin-zero-Fin {k} f))
-- has-repetition-of-values-is-not-injective-Fin k (succ-ℕ l) f H with
--   is-decidable-is-repetition-of-values-Fin (succ-ℕ l) k f (inr star)
-- ... | inl r = pair (inr star) r
-- ... | inr g =
--   α (has-repetition-of-values-is-not-injective-Fin k l (f ∘ inl) K)
--   where
--   K : is-not-injective (f ∘ inl)
--   K I = H (λ {x} {y} → J x y)
--     where
--     J : (x y : Fin (succ-ℕ l)) → Id (f x) (f y) → Id x y
--     J (inl x) (inl y) p = ap inl (I p)
--     J (inl x) (inr star) p =
--       ex-falso (g (triple (inl x) (Eq-Fin-eq (succ-ℕ l)) (inv p)))
--     J (inr star) (inl y) p =
--       ex-falso (g (triple (inl y) (Eq-Fin-eq (succ-ℕ l)) p))
--     J (inr star) (inr star) p = refl
--     α : has-repetition-of-values (f ∘ inl) → has-repetition-of-values f
--     α (pair x (pair y (pair h q))) =
--       pair (inl x) (pair (inl y) (pair (λ r → h (is-injective-inl r)) q))
```
