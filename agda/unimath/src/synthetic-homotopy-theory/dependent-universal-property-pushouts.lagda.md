# The dependent universal property of pushouts

```agda
module synthetic-homotopy-theory.dependent-universal-property-pushouts where
```

<details><summary>Imports</summary>

```agda
open import foundation.action-on-identifications-dependent-functions
open import foundation.contractible-maps
open import foundation.contractible-types
open import foundation.dependent-pair-types
open import foundation.equality-dependent-pair-types
open import foundation.equivalences
open import foundation.fibers-of-maps
open import foundation.function-extensionality
open import foundation.function-types
open import foundation.functoriality-dependent-pair-types
open import foundation.homotopies
open import foundation.identity-types
open import foundation.pullbacks
open import foundation.transport
open import foundation.universe-levels

open import synthetic-homotopy-theory.cocones-under-spans
open import synthetic-homotopy-theory.dependent-cocones-under-spans
open import synthetic-homotopy-theory.dependent-pullback-property-pushouts
open import synthetic-homotopy-theory.induction-principle-pushouts
```

</details>

## Idea

The **dependent universal property of pushouts** asserts that every section of a
type family over a [pushout](synthetic-homotopy-theory.pushouts.md) corresponds
in a canonical way uniquely to a
[dependent cocone](synthetic-homotopy-theory.dependent-cocones-under-spans.md)
over the [cocone structure](synthetic-homotopy-theory.cocones-under-spans.md) on
the pushout.

## Definition

### The dependent universal property of pushouts

```agda
dependent-universal-property-pushout :
  {l1 l2 l3 l4 : Level} (l : Level) {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  UU (l1 ⊔ l2 ⊔ l3 ⊔ l4 ⊔ lsuc l)
dependent-universal-property-pushout l f g {X} c =
  (P : X → UU l) → is-equiv (dependent-cocone-map f g c P)
```

## Properties

### Sections defined by the dependent universal property are unique up to homotopy

```agda
abstract
  uniqueness-dependent-universal-property-pushout :
    { l1 l2 l3 l4 l : Level} {S : UU l1} {A : UU l2} {B : UU l3} {X : UU l4} →
    ( f : S → A) (g : S → B) (c : cocone f g X)
    ( dup-c : dependent-universal-property-pushout l f g c) →
    ( P : X → UU l) ( h : dependent-cocone f g c P) →
    is-contr
      ( Σ ( (x : X) → P x)
          ( λ k →
            htpy-dependent-cocone f g c P (dependent-cocone-map f g c P k) h))
  uniqueness-dependent-universal-property-pushout f g c dup-c P h =
    is-contr-is-equiv'
      ( fib (dependent-cocone-map f g c P) h)
      ( tot
        ( λ k →
          htpy-eq-dependent-cocone f g c P (dependent-cocone-map f g c P k) h))
      ( is-equiv-tot-is-fiberwise-equiv
        ( λ k →
          is-equiv-htpy-eq-dependent-cocone f g c P
            ( dependent-cocone-map f g c P k)
            ( h)))
      ( is-contr-map-is-equiv (dup-c P) h)
```

### The induction principle of pushouts is equivalent to the dependent universal property of pushouts

#### The induction principle of pushouts implies the dependent universal property of pushouts

```agda
htpy-eq-dependent-cocone-map :
  { l1 l2 l3 l4 l : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  ( f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  ( H : induction-principle-pushout l f g c)
  { P : X → UU l} (h h' : (x : X) → P x) →
  dependent-cocone-map f g c P h ＝ dependent-cocone-map f g c P h' → h ~ h'
htpy-eq-dependent-cocone-map f g c ind-c {P} h h' p =
  ind-induction-principle-pushout f g c ind-c
    ( λ x → Id (h x) (h' x))
    ( pair
      ( horizontal-htpy-eq-dependent-cocone f g c P
        ( dependent-cocone-map f g c P h)
        ( dependent-cocone-map f g c P h')
        ( p))
      ( pair
        ( vertical-htpy-eq-dependent-cocone f g c P
          ( dependent-cocone-map f g c P h)
          ( dependent-cocone-map f g c P h')
          ( p))
        ( λ s →
          map-compute-dependent-identification-eq-value h h'
            ( coherence-square-cocone f g c s)
            ( horizontal-htpy-eq-dependent-cocone f g c P
              ( dependent-cocone-map f g c P h)
              ( dependent-cocone-map f g c P h')
              ( p)
              ( f s))
            ( vertical-htpy-eq-dependent-cocone f g c P
              ( dependent-cocone-map f g c P h)
              ( dependent-cocone-map f g c P h')
              ( p)
              ( g s))
            ( coherence-square-htpy-eq-dependent-cocone f g c P
              ( dependent-cocone-map f g c P h)
              ( dependent-cocone-map f g c P h')
              ( p)
              ( s)))))

dependent-universal-property-pushout-induction-principle-pushout :
  {l1 l2 l3 l4 : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  ((l : Level) → induction-principle-pushout l f g c) →
  ((l : Level) → dependent-universal-property-pushout l f g c)
dependent-universal-property-pushout-induction-principle-pushout
  f g c ind-c l P =
  is-equiv-has-inverse
    ( ind-induction-principle-pushout f g c (ind-c l) P)
    ( pr2 (ind-c l P))
    ( λ h →
      eq-htpy (htpy-eq-dependent-cocone-map f g c
        ( ind-c l)
        ( ind-induction-principle-pushout f g c
          ( ind-c l)
          ( P)
          ( dependent-cocone-map f g c P h))
        ( h)
        ( pr2 (ind-c l P) (dependent-cocone-map f g c P h))))
```

#### The dependent universal property of pushouts implies the induction principle of pushouts

```agda
induction-principle-pushout-dependent-universal-property-pushout :
  {l1 l2 l3 l4 : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  ((l : Level) → dependent-universal-property-pushout l f g c) →
  ((l : Level) → induction-principle-pushout l f g c)
induction-principle-pushout-dependent-universal-property-pushout
  f g c dup-c l P =
  pr1 (dup-c l P)
```

### The dependent pullback property of pushouts is equivalent to the dependent universal property of pushouts

#### The dependent universal property of pushouts implies the dependent pullback property of pushouts

```agda
triangle-dependent-pullback-property-pushout :
  {l1 l2 l3 l4 l5 : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) (P : X → UU l5) →
  let i = pr1 c
      j = pr1 (pr2 c)
      H = pr2 (pr2 c)
  in
  ( dependent-cocone-map f g c P) ~
  ( ( tot (λ h → tot (λ h' → htpy-eq))) ∘
    ( gap
      ( λ (h : (a : A) → P (i a)) → λ s → tr P (H s) (h (f s)))
      ( λ (h : (b : B) → P (j b)) → λ s → h (g s))
      ( cone-dependent-pullback-property-pushout f g c P)))
triangle-dependent-pullback-property-pushout f g (pair i (pair j H)) P h =
  eq-pair-Σ refl (eq-pair-Σ refl (inv (is-section-eq-htpy (apd h ∘ H))))

dependent-pullback-property-dependent-universal-property-pushout :
  {l1 l2 l3 l4 : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  ((l : Level) → dependent-universal-property-pushout l f g c) →
  ((l : Level) → dependent-pullback-property-pushout l f g c)
dependent-pullback-property-dependent-universal-property-pushout
  f g (pair i (pair j H)) I l P =
  let c = (pair i (pair j H)) in
  is-equiv-right-factor-htpy
    ( dependent-cocone-map f g c P)
    ( tot (λ h → tot (λ h' → htpy-eq)))
    ( gap
      ( λ h x → tr P (H x) (h (f x)))
      ( _∘ g)
      ( cone-dependent-pullback-property-pushout f g c P))
    ( triangle-dependent-pullback-property-pushout f g c P)
    ( is-equiv-tot-is-fiberwise-equiv
      ( λ h → is-equiv-tot-is-fiberwise-equiv
        ( λ h' → funext (λ x → tr P (H x) (h (f x))) (h' ∘ g))))
    ( I l P)
```

#### The dependent pullback property of pushouts implies the dependent universal property of pushouts

```agda
dependent-universal-property-dependent-pullback-property-pushout :
  {l1 l2 l3 l4 : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  ((l : Level) → dependent-pullback-property-pushout l f g c) →
  ((l : Level) → dependent-universal-property-pushout l f g c)
dependent-universal-property-dependent-pullback-property-pushout
  f g (pair i (pair j H)) dpullback-c l P =
  let c = (pair i (pair j H)) in
  is-equiv-comp-htpy
    ( dependent-cocone-map f g c P)
    ( tot (λ h → tot (λ h' → htpy-eq)))
    ( gap
      ( λ h x → tr P (H x) (h (f x)))
      ( _∘ g)
      ( cone-dependent-pullback-property-pushout f g c P))
    ( triangle-dependent-pullback-property-pushout f g c P)
    ( dpullback-c l P)
    ( is-equiv-tot-is-fiberwise-equiv
      ( λ h → is-equiv-tot-is-fiberwise-equiv
        ( λ h' → funext (λ x → tr P (H x) (h (f x))) (h' ∘ g))))
```
