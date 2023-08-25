# Diagonal maps of types

```agda
module foundation-core.diagonal-maps-of-types where
```

<details><summary>Imports</summary>

```agda
open import foundation.action-on-identifications-functions
open import foundation.dependent-pair-types
open import foundation.equality-cartesian-product-types
open import foundation.universe-levels

open import foundation-core.cartesian-product-types
open import foundation-core.equivalences
open import foundation-core.fibers-of-maps
open import foundation-core.function-types
open import foundation-core.homotopies
open import foundation-core.identity-types
open import foundation-core.propositions
```

</details>

## Idea

The diagonal map `δ : A → A × A` of `A` is the map that includes `A` as the
diagonal into `A × A`.

## Definition

```agda
module _
  {l : Level} (A : UU l)
  where

  diagonal : A → A × A
  pr1 (diagonal x) = x
  pr2 (diagonal x) = x
```

## Properties

### The action on paths of a diagonal

```agda
ap-diagonal :
  {l : Level} {A : UU l} {x y : A} (p : x ＝ y) →
  ap (diagonal A) p ＝ eq-pair p p
ap-diagonal refl = refl
```

### If the diagonal of `A` is an equivalence, then `A` is a proposition

```agda
module _
  {l : Level} (A : UU l)
  where

  abstract
    is-prop-is-equiv-diagonal : is-equiv (diagonal A) → is-prop A
    is-prop-is-equiv-diagonal is-equiv-d =
      is-prop-all-elements-equal
        ( λ x y →
          ( inv (ap pr1 (is-section-map-inv-is-equiv is-equiv-d (pair x y)))) ∙
          ( ap pr2 (is-section-map-inv-is-equiv is-equiv-d (pair x y))))

  equiv-diagonal-is-prop :
    is-prop A → A ≃ (A × A)
  pr1 (equiv-diagonal-is-prop is-prop-A) = diagonal A
  pr2 (equiv-diagonal-is-prop is-prop-A) =
    is-equiv-has-inverse
      ( pr1)
      ( λ pair-a → eq-pair (eq-is-prop is-prop-A) (eq-is-prop is-prop-A))
      ( λ a → eq-is-prop is-prop-A)
```

### The fibers of the diagonal map

```agda
module _
  {l : Level} (A : UU l)
  where

  eq-fib-diagonal : (t : A × A) → fib (diagonal A) t → pr1 t ＝ pr2 t
  eq-fib-diagonal (pair x y) (pair z α) = (inv (ap pr1 α)) ∙ (ap pr2 α)

  fib-diagonal-eq : (t : A × A) → pr1 t ＝ pr2 t → fib (diagonal A) t
  pr1 (fib-diagonal-eq (pair x y) β) = x
  pr2 (fib-diagonal-eq (pair x y) β) = eq-pair refl β

  is-section-fib-diagonal-eq :
    (t : A × A) → ((eq-fib-diagonal t) ∘ (fib-diagonal-eq t)) ~ id
  is-section-fib-diagonal-eq (pair x .x) refl = refl

  is-retraction-fib-diagonal-eq :
    (t : A × A) → ((fib-diagonal-eq t) ∘ (eq-fib-diagonal t)) ~ id
  is-retraction-fib-diagonal-eq .(pair z z) (pair z refl) = refl

  abstract
    is-equiv-eq-fib-diagonal : (t : A × A) → is-equiv (eq-fib-diagonal t)
    is-equiv-eq-fib-diagonal t =
      is-equiv-has-inverse
        ( fib-diagonal-eq t)
        ( is-section-fib-diagonal-eq t)
        ( is-retraction-fib-diagonal-eq t)
```
