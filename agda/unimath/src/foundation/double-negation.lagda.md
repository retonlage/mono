# Double negation

```agda
module foundation.double-negation where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.negation
open import foundation.propositional-truncations
open import foundation.universe-levels

open import foundation-core.cartesian-product-types
open import foundation-core.coproduct-types
open import foundation-core.empty-types
open import foundation-core.function-types
open import foundation-core.propositions
```

</details>

## Definition

We define double negation and triple negation

```agda
¬¬ : {l : Level} → UU l → UU l
¬¬ P = ¬ (¬ P)

¬¬¬ : {l : Level} → UU l → UU l
¬¬¬ P = ¬ (¬ (¬ P))
```

We also define the introduction rule for double negation, and the action on maps
of double negation.

```agda
intro-double-negation : {l : Level} {P : UU l} → P → ¬¬ P
intro-double-negation p f = f p

map-double-negation :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} → (P → Q) → (¬¬ P → ¬¬ Q)
map-double-negation f = map-neg (map-neg f)
```

## Properties

### The double negation of a type is a proposition

```agda
double-negation-Prop' :
  {l : Level} (A : UU l) → Prop l
double-negation-Prop' A = neg-Prop' (¬ A)

double-negation-Prop :
  {l : Level} (P : Prop l) → Prop l
double-negation-Prop P = double-negation-Prop' (type-Prop P)

is-prop-double-negation :
  {l : Level} {A : UU l} → is-prop (¬¬ A)
is-prop-double-negation = is-prop-neg
```

### Double negations of classical laws

```agda
double-negation-double-negation-elim :
  {l : Level} {P : UU l} → ¬¬ (¬¬ P → P)
double-negation-double-negation-elim {P = P} f =
  ( λ (np : ¬ P) → f (λ (nnp : ¬¬ P) → ex-falso (nnp np)))
  ( λ (p : P) → f (λ (nnp : ¬¬ P) → p))

double-negation-Peirces-law :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} → ¬¬ (((P → Q) → P) → P)
double-negation-Peirces-law {P = P} f =
  ( λ (np : ¬ P) → f (λ h → h (λ p → ex-falso (np p))))
  ( λ (p : P) → f (λ _ → p))

double-negation-linearity-implication :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} →
  ¬¬ ((P → Q) + (Q → P))
double-negation-linearity-implication {P = P} {Q = Q} f =
  ( λ (np : ¬ P) →
    map-neg (inl {A = P → Q} {B = Q → P}) f (λ p → ex-falso (np p)))
  ( λ (p : P) → map-neg (inr {A = P → Q} {B = Q → P}) f (λ _ → p))
```

### Cases of double negation elimination

```agda
double-negation-elim-neg : {l : Level} (P : UU l) → ¬¬¬ P → ¬ P
double-negation-elim-neg P f p = f (λ g → g p)

double-negation-elim-prod :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} →
  ¬¬ ((¬¬ P) × (¬¬ Q)) → (¬¬ P) × (¬¬ Q)
pr1 (double-negation-elim-prod {P = P} {Q = Q} f) =
  double-negation-elim-neg (¬ P) (map-double-negation pr1 f)
pr2 (double-negation-elim-prod {P = P} {Q = Q} f) =
  double-negation-elim-neg (¬ Q) (map-double-negation pr2 f)

double-negation-elim-exp :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} →
  ¬¬ (P → ¬¬ Q) → (P → ¬¬ Q)
double-negation-elim-exp {P = P} {Q = Q} f p =
  double-negation-elim-neg
    ( ¬ Q)
    ( map-double-negation (λ (g : P → ¬¬ Q) → g p) f)

double-negation-elim-forall :
  {l1 l2 : Level} {P : UU l1} {Q : P → UU l2} →
  ¬¬ ((p : P) → ¬¬ (Q p)) → (p : P) → ¬¬ (Q p)
double-negation-elim-forall {P = P} {Q = Q} f p =
  double-negation-elim-neg
    ( ¬ (Q p))
    ( map-double-negation (λ (g : (u : P) → ¬¬ (Q u)) → g p) f)
```

### Maps into double negations extend along `intro-double-negation`

```agda
double-negation-extend :
  {l1 l2 : Level} {P : UU l1} {Q : UU l2} →
  (P → ¬¬ Q) → (¬¬ P → ¬¬ Q)
double-negation-extend {P = P} {Q = Q} f =
  double-negation-elim-neg (¬ Q) ∘ (map-double-negation f)
```

### The double negation of a type is logically equivalent to the double negation of its propositional truncation

```agda
abstract
  double-negation-double-negation-type-trunc-Prop :
    {l : Level} (A : UU l) → ¬¬ (type-trunc-Prop A) → ¬¬ A
  double-negation-double-negation-type-trunc-Prop A =
    double-negation-extend
      ( map-universal-property-trunc-Prop
        ( double-negation-Prop' A)
        ( intro-double-negation))

abstract
  double-negation-type-trunc-Prop-double-negation :
    {l : Level} {A : UU l} → ¬¬ A → ¬¬ (type-trunc-Prop A)
  double-negation-type-trunc-Prop-double-negation =
    map-double-negation unit-trunc-Prop
```
