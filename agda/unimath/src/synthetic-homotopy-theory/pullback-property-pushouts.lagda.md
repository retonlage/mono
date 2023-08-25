# The pullback property of pushouts

```agda
module synthetic-homotopy-theory.pullback-property-pushouts where
```

<details><summary>Imports</summary>

```agda
open import foundation.cones-over-cospans
open import foundation.dependent-pair-types
open import foundation.function-types
open import foundation.functoriality-function-types
open import foundation.pullbacks
open import foundation.universe-levels

open import synthetic-homotopy-theory.cocones-under-spans
```

</details>

## Idea

The
[universal property of the pushout](synthetic-homotopy-theory.universal-property-pushouts.md)
of a span `S` can also be stated as a
[pullback property](foundation-core.universal-property-pullbacks.md): a cocone
`c ≐ (i , j , H)` with vertex `X` satisfies the universal property of the
pushout of `S` if and only if the square

```text
  Y^X -----> Y^B
   |          |
   |          |
   V          V
  Y^A -----> Y^S
```

is a pullback square for every type `Y`. Below, we first define the cone of this
commuting square, and then we introduce the type pullback-property-pushout,
which states that the above square is a pullback.

## Definition

### The pullback property of pushouts

```agda
cone-pullback-property-pushout :
  {l1 l2 l3 l4 l : Level} {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) (Y : UU l) →
  cone (_∘ f) (_∘ g) (X → Y)
pr1 (cone-pullback-property-pushout f g c Y) =
  precomp (horizontal-map-cocone f g c) Y
pr1 (pr2 (cone-pullback-property-pushout f g c Y)) =
  precomp (vertical-map-cocone f g c) Y
pr2 (pr2 (cone-pullback-property-pushout f g c Y)) =
  htpy-precomp (coherence-square-cocone f g c) Y

pullback-property-pushout :
  {l1 l2 l3 l4 : Level} (l : Level) {S : UU l1} {A : UU l2} {B : UU l3}
  (f : S → A) (g : S → B) {X : UU l4} (c : cocone f g X) →
  UU (l1 ⊔ l2 ⊔ l3 ⊔ l4 ⊔ lsuc l)
pullback-property-pushout l f g c =
  (Y : UU l) →
  is-pullback
    ( precomp f Y)
    ( precomp g Y)
    ( cone-pullback-property-pushout f g c Y)
```
