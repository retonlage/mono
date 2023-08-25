# Representable functors between precategories

```agda
module category-theory.representable-functors-precategories where
```

<details><summary>Imports</summary>

```agda
open import category-theory.functors-precategories
open import category-theory.natural-transformations-precategories
open import category-theory.precategories

open import foundation.category-of-sets
open import foundation.dependent-pair-types
open import foundation.function-extensionality
open import foundation.homotopies
open import foundation.universe-levels
```

</details>

## Idea

Given a precategory `C` and an object `c`, there is a functor from `C` to the
precategory of Sets represented by `c` that:

- sends an object `x` of `C` to the set `hom c x` and
- sends a morphism `g : hom x y` of `C` to the function `hom c x → hom c y`
  defined by postcomposition with `g`.

The functoriality axioms follow, by function extensionality, from associativity
and the left unit law for the precategory `C`.

## Definition

```agda
rep-functor-Precategory :
  {l1 l2 : Level} (C : Precategory l1 l2) (c : obj-Precategory C) →
  functor-Precategory C (Set-Precategory l2)
pr1 (rep-functor-Precategory C c) = hom-Precategory C c
pr1 (pr2 (rep-functor-Precategory C c)) g = postcomp-hom-Precategory C g c
pr1 (pr2 (pr2 (rep-functor-Precategory C c))) h g =
  eq-htpy (associative-comp-hom-Precategory C h g)
pr2 (pr2 (pr2 (rep-functor-Precategory C c))) _ =
  eq-htpy (left-unit-law-comp-hom-Precategory C)
```

## Natural transformations between representable functors

A morphism `f : hom b c` in a precategory `C` defines a natural transformation
from the functor represented by `c` to the functor represented by `b`. Its
components `hom c x → hom b x` are defined by precomposition with `f`.

```agda
rep-natural-transformation-Precategory :
  {l1 l2 : Level} (C : Precategory l1 l2) (b c : obj-Precategory C)
  (f : type-hom-Precategory C b c) →
  natural-transformation-Precategory
    ( C)
    ( Set-Precategory l2)
    ( rep-functor-Precategory C c)
    ( rep-functor-Precategory C b)
pr1 (rep-natural-transformation-Precategory C b c f) =
  precomp-hom-Precategory C f
pr2 (rep-natural-transformation-Precategory C b c f) h =
  eq-htpy (inv-htpy (λ g → associative-comp-hom-Precategory C h g f))
```
