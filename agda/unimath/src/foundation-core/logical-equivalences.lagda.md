# Logical equivalences

```agda
module foundation-core.logical-equivalences where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.universe-levels

open import foundation-core.cartesian-product-types
open import foundation-core.equivalences
open import foundation-core.function-types
open import foundation-core.propositions
```

</details>

## Idea

Logical equivalences between two types `A` and `B` consist of a map `A → B` and
a map `B → A`. The type of logical equivalences between types is the
Curry-Howard interpretation of logical equivalences between propositions.

## Definition

### Logical equivalences between types

```agda
_↔_ : {l1 l2 : Level} → UU l1 → UU l2 → UU (l1 ⊔ l2)
A ↔ B = (A → B) × (B → A)

module _
  {l1 l2 : Level} {A : UU l1} {B : UU l2} (H : A ↔ B)
  where

  forward-implication : A → B
  forward-implication = pr1 H

  backward-implication : B → A
  backward-implication = pr2 H
```

### Logical equivalences between propositions

```agda
_⇔_ :
  {l1 l2 : Level} → Prop l1 → Prop l2 → UU (l1 ⊔ l2)
P ⇔ Q = type-Prop P ↔ type-Prop Q

is-prop-iff-Prop :
  {l1 l2 : Level} (P : Prop l1) (Q : Prop l2) →
  is-prop (P ⇔ Q)
is-prop-iff-Prop P Q =
  is-prop-prod
    ( is-prop-function-type (is-prop-type-Prop Q))
    ( is-prop-function-type (is-prop-type-Prop P))

iff-Prop :
  {l1 l2 : Level} → Prop l1 → Prop l2 → Prop (l1 ⊔ l2)
pr1 (iff-Prop P Q) = P ⇔ Q
pr2 (iff-Prop P Q) = is-prop-iff-Prop P Q
```

### Composition of logical equivalences

```agda
_∘iff_ :
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} {C : UU l3} →
  (B ↔ C) → (A ↔ B) → (A ↔ C)
pr1 (pair g1 g2 ∘iff pair f1 f2) = g1 ∘ f1
pr2 (pair g1 g2 ∘iff pair f1 f2) = f2 ∘ g2
```

### Inverting a logical equivalence

```agda
inv-iff :
  {l1 l2 : Level} {A : UU l1} {B : UU l2} → (A ↔ B) → (B ↔ A)
pr1 (inv-iff (f , g)) = g
pr2 (inv-iff (f , g)) = f
```

### Logical equivalences between propositions induce equivalences

```agda
module _
  {l1 l2 : Level} (P : Prop l1) (Q : Prop l2)
  where

  equiv-iff' : (P ⇔ Q) → (type-Prop P ≃ type-Prop Q)
  pr1 (equiv-iff' t) = pr1 t
  pr2 (equiv-iff' t) = is-equiv-is-prop (pr2 P) (pr2 Q) (pr2 t)

  equiv-iff :
    (type-Prop P → type-Prop Q) → (type-Prop Q → type-Prop P) →
    type-Prop P ≃ type-Prop Q
  equiv-iff f g = equiv-iff' (pair f g)

iff-equiv : {l1 l2 : Level} {A : UU l1} {B : UU l2} → (A ≃ B) → (A ↔ B)
pr1 (iff-equiv e) = map-equiv e
pr2 (iff-equiv e) = map-inv-equiv e
```

## Logical equivalences between dependent function types

```agda
module _
  {l1 l2 l3 : Level} {I : UU l1} {A : I → UU l2} {B : I → UU l3}
  where

  iff-Π : ((i : I) → A i ↔ B i) → ((i : I) → A i) ↔ ((i : I) → B i)
  pr1 (iff-Π e) a i = forward-implication (e i) (a i)
  pr2 (iff-Π e) b i = backward-implication (e i) (b i)
```

## Reasoning with logical equivalences

Logical equivalences can be constructed by equational reasoning in the following
way:

```text
logical-equivalence-reasoning
  X ↔ Y by equiv-1
    ↔ Z by equiv-2
    ↔ V by equiv-3
```

```agda
infixl 1 logical-equivalence-reasoning_
infixl 0 step-logical-equivalence-reasoning

logical-equivalence-reasoning_ :
  {l1 : Level} (X : UU l1) → X ↔ X
logical-equivalence-reasoning X = pair id id

step-logical-equivalence-reasoning :
  {l1 l2 l3 : Level} {X : UU l1} {Y : UU l2} →
  (X ↔ Y) → (Z : UU l3) → (Y ↔ Z) → (X ↔ Z)
step-logical-equivalence-reasoning e Z f = f ∘iff e

syntax step-logical-equivalence-reasoning e Z f = e ↔ Z by f
```
