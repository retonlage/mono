# Least upper bounds in posets

```agda
module order-theory.least-upper-bounds-posets where
```

<details><summary>Imports</summary>

```agda
open import foundation.action-on-identifications-functions
open import foundation.dependent-pair-types
open import foundation.identity-types
open import foundation.logical-equivalences
open import foundation.propositions
open import foundation.subtypes
open import foundation.universe-levels

open import order-theory.posets
open import order-theory.upper-bounds-posets
```

</details>

## Idea

A **least upper bound** of `a` and `b` in a poset `P` is an element `x` such
that the logical equivalence

```text
  ((a ≤ y) ∧ (b ≤ y)) ⇔ (x ≤ y)
```

holds for every element `y` in `P`. Similarly, a **least upper bound** of a
family `a : I → P` of elements of `P` is an element `x` of `P` such that the
logical equivalence

```text
  (∀ᵢ (aᵢ ≤ y)) ⇔ (x ≤ y)
```

holds for every element `y` in `P`.

## Definitions

### The predicate of being a least binary upper bound of two elements in a poset

```agda
module _
  {l1 l2 : Level} (P : Poset l1 l2) (a b : type-Poset P)
  where

  is-least-binary-upper-bound-Poset-Prop : type-Poset P → Prop (l1 ⊔ l2)
  is-least-binary-upper-bound-Poset-Prop x =
    Π-Prop
      ( type-Poset P)
      ( λ y →
        iff-Prop
          ( is-binary-upper-bound-Poset-Prop P a b y)
          ( leq-Poset-Prop P x y))

  is-least-binary-upper-bound-Poset : type-Poset P → UU (l1 ⊔ l2)
  is-least-binary-upper-bound-Poset x =
    type-Prop (is-least-binary-upper-bound-Poset-Prop x)

  is-prop-is-least-binary-upper-bound-Poset :
    (x : type-Poset P) →
    is-prop (is-least-binary-upper-bound-Poset x)
  is-prop-is-least-binary-upper-bound-Poset x =
    is-prop-type-Prop (is-least-binary-upper-bound-Poset-Prop x)

module _
  {l1 l2 : Level} (P : Poset l1 l2) {a b : type-Poset P}
  where

  forward-implication-is-least-binary-upper-bound-Poset :
    {x y : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    is-binary-upper-bound-Poset P a b y → leq-Poset P x y
  forward-implication-is-least-binary-upper-bound-Poset H =
    forward-implication (H _)

  backward-implication-is-least-binary-upper-bound-Poset :
    {x y : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    leq-Poset P x y → is-binary-upper-bound-Poset P a b y
  backward-implication-is-least-binary-upper-bound-Poset {x} {y} H =
    backward-implication (H y)

  prove-is-least-binary-upper-bound-Poset :
    {x : type-Poset P} →
    is-binary-upper-bound-Poset P a b x →
    ( (y : type-Poset P) →
      is-binary-upper-bound-Poset P a b y → leq-Poset P x y) →
    is-least-binary-upper-bound-Poset P a b x
  pr1 (prove-is-least-binary-upper-bound-Poset H K y) L = K y L
  pr2 (prove-is-least-binary-upper-bound-Poset H K y) L =
    is-binary-upper-bound-leq-Poset P H L

  is-binary-upper-bound-is-least-binary-upper-bound-Poset :
    {x : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    is-binary-upper-bound-Poset P a b x
  is-binary-upper-bound-is-least-binary-upper-bound-Poset H =
    backward-implication-is-least-binary-upper-bound-Poset H
      ( refl-leq-Poset P _)

  leq-left-is-least-binary-upper-bound-Poset :
    {x : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    leq-Poset P a x
  leq-left-is-least-binary-upper-bound-Poset H =
    leq-left-is-binary-upper-bound-Poset P
      ( is-binary-upper-bound-is-least-binary-upper-bound-Poset H)

  leq-right-is-least-binary-upper-bound-Poset :
    {x : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    leq-Poset P b x
  leq-right-is-least-binary-upper-bound-Poset H =
    leq-right-is-binary-upper-bound-Poset P
      ( is-binary-upper-bound-is-least-binary-upper-bound-Poset H)
```

### The proposition that two elements have a least upper bound

```agda
module _
  {l1 l2 : Level} (P : Poset l1 l2) (a b : type-Poset P)
  where

  has-least-binary-upper-bound-Poset : UU (l1 ⊔ l2)
  has-least-binary-upper-bound-Poset =
    Σ (type-Poset P) (is-least-binary-upper-bound-Poset P a b)

  all-elements-equal-has-least-binary-upper-bound-Poset :
    all-elements-equal has-least-binary-upper-bound-Poset
  all-elements-equal-has-least-binary-upper-bound-Poset
    (pair u H) (pair v K) =
    eq-type-subtype
      ( is-least-binary-upper-bound-Poset-Prop P a b)
      ( antisymmetric-leq-Poset P u v
        ( forward-implication-is-least-binary-upper-bound-Poset P H
          ( is-binary-upper-bound-is-least-binary-upper-bound-Poset P K))
        ( forward-implication-is-least-binary-upper-bound-Poset P K
          ( is-binary-upper-bound-is-least-binary-upper-bound-Poset P H)))

  is-prop-has-least-binary-upper-bound-Poset :
    is-prop has-least-binary-upper-bound-Poset
  is-prop-has-least-binary-upper-bound-Poset =
    is-prop-all-elements-equal
      all-elements-equal-has-least-binary-upper-bound-Poset

  has-least-binary-upper-bound-Poset-Prop : Prop (l1 ⊔ l2)
  pr1 has-least-binary-upper-bound-Poset-Prop =
    has-least-binary-upper-bound-Poset
  pr2 has-least-binary-upper-bound-Poset-Prop =
    is-prop-has-least-binary-upper-bound-Poset

module _
  {l1 l2 : Level} (P : Poset l1 l2) {a b : type-Poset P}
  where

  eq-is-least-binary-upper-bound-Poset :
    {x y : type-Poset P} →
    is-least-binary-upper-bound-Poset P a b x →
    is-least-binary-upper-bound-Poset P a b y →
    x ＝ y
  eq-is-least-binary-upper-bound-Poset {x} {y} H K =
    ap
      ( pr1)
      ( all-elements-equal-has-least-binary-upper-bound-Poset P a b
        ( x , H)
        ( y , K))
```

### Least upper bounds of families of elements

```agda
module _
  {l1 l2 l3 : Level} (P : Poset l1 l2) {I : UU l3} (a : I → type-Poset P)
  where

  is-least-upper-bound-family-of-elements-Poset-Prop :
    type-Poset P → Prop (l1 ⊔ l2 ⊔ l3)
  is-least-upper-bound-family-of-elements-Poset-Prop x =
    Π-Prop
      ( type-Poset P)
      ( λ y →
        iff-Prop
          ( Π-Prop I (λ i → leq-Poset-Prop P (a i) y))
          ( leq-Poset-Prop P x y))

  is-least-upper-bound-family-of-elements-Poset :
    type-Poset P → UU (l1 ⊔ l2 ⊔ l3)
  is-least-upper-bound-family-of-elements-Poset z =
    type-Prop (is-least-upper-bound-family-of-elements-Poset-Prop z)

  is-prop-is-least-upper-bound-family-of-elements-Poset :
    (z : type-Poset P) →
    is-prop (is-least-upper-bound-family-of-elements-Poset z)
  is-prop-is-least-upper-bound-family-of-elements-Poset z =
    is-prop-type-Prop (is-least-upper-bound-family-of-elements-Poset-Prop z)

module _
  {l1 l2 l3 : Level} (P : Poset l1 l2) {I : UU l3} {a : I → type-Poset P}
  where

  forward-implication-is-least-upper-bound-family-of-elements-Poset :
    {x y : type-Poset P} →
    is-least-upper-bound-family-of-elements-Poset P a x →
    is-upper-bound-family-of-elements-Poset P a y → leq-Poset P x y
  forward-implication-is-least-upper-bound-family-of-elements-Poset H =
    forward-implication (H _)

  backward-implication-is-least-upper-bound-family-of-elements-Poset :
    {x y : type-Poset P} →
    is-least-upper-bound-family-of-elements-Poset P a x →
    leq-Poset P x y → is-upper-bound-family-of-elements-Poset P a y
  backward-implication-is-least-upper-bound-family-of-elements-Poset H =
    backward-implication (H _)

  is-upper-bound-is-least-upper-bound-family-of-elements-Poset :
    {x : type-Poset P} →
    is-least-upper-bound-family-of-elements-Poset P a x →
    is-upper-bound-family-of-elements-Poset P a x
  is-upper-bound-is-least-upper-bound-family-of-elements-Poset H =
    backward-implication-is-least-upper-bound-family-of-elements-Poset H
      ( refl-leq-Poset P _)
```

### The proposition that a family of elements has a least upper bound

```agda
module _
  {l1 l2 l3 : Level} (P : Poset l1 l2) {I : UU l3} (a : I → type-Poset P)
  where

  has-least-upper-bound-family-of-elements-Poset : UU (l1 ⊔ l2 ⊔ l3)
  has-least-upper-bound-family-of-elements-Poset =
    Σ (type-Poset P) (is-least-upper-bound-family-of-elements-Poset P a)

  all-elements-equal-has-least-upper-bound-family-of-elements-Poset :
    all-elements-equal has-least-upper-bound-family-of-elements-Poset
  all-elements-equal-has-least-upper-bound-family-of-elements-Poset
    ( x , H) (y , K) =
    eq-type-subtype
      ( is-least-upper-bound-family-of-elements-Poset-Prop P a)
      ( antisymmetric-leq-Poset P x y
        ( forward-implication-is-least-upper-bound-family-of-elements-Poset
          ( P)
          ( H)
          ( is-upper-bound-is-least-upper-bound-family-of-elements-Poset
            ( P)
            ( K)))
        ( forward-implication-is-least-upper-bound-family-of-elements-Poset
          ( P)
          ( K)
          ( is-upper-bound-is-least-upper-bound-family-of-elements-Poset
            ( P)
            ( H))))

  is-prop-has-least-upper-bound-family-of-elements-Poset :
    is-prop has-least-upper-bound-family-of-elements-Poset
  is-prop-has-least-upper-bound-family-of-elements-Poset =
    is-prop-all-elements-equal
      all-elements-equal-has-least-upper-bound-family-of-elements-Poset

  has-least-upper-bound-family-of-elements-Poset-Prop : Prop (l1 ⊔ l2 ⊔ l3)
  pr1 has-least-upper-bound-family-of-elements-Poset-Prop =
    has-least-upper-bound-family-of-elements-Poset
  pr2 has-least-upper-bound-family-of-elements-Poset-Prop =
    is-prop-has-least-upper-bound-family-of-elements-Poset

module _
  {l1 l2 l3 : Level} (P : Poset l1 l2) {I : UU l3} {a : I → type-Poset P}
  where

  eq-is-least-upper-bound-family-of-elements-Poset :
    {x y : type-Poset P} →
    is-least-upper-bound-family-of-elements-Poset P a x →
    is-least-upper-bound-family-of-elements-Poset P a y →
    x ＝ y
  eq-is-least-upper-bound-family-of-elements-Poset H K =
    ap
      ( pr1)
      ( all-elements-equal-has-least-upper-bound-family-of-elements-Poset
        ( P)
        ( a)
        ( _ , H)
        ( _ , K))
```
