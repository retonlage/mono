# Large precategories

```agda
module category-theory.large-precategories where
```

<details><summary>Imports</summary>

```agda
open import category-theory.precategories

open import foundation.action-on-identifications-binary-functions
open import foundation.dependent-pair-types
open import foundation.identity-types
open import foundation.sets
open import foundation.universe-levels
```

</details>

## Idea

A large precategory is a precategory where we don't fix a universe for the type
of objects or morphisms. (This cannot be done with Σ-types, we must use a record
type.)

## Definition

### Large precategories

```agda
record
  Large-Precategory (α : Level → Level) (β : Level → Level → Level) : UUω where
  constructor make-Large-Precategory
  field
    obj-Large-Precategory :
      (l : Level) → UU (α l)

    hom-Large-Precategory :
      {l1 l2 : Level} → obj-Large-Precategory l1 → obj-Large-Precategory l2 →
      Set (β l1 l2)

    comp-hom-Large-Precategory :
      {l1 l2 l3 : Level}
      {X : obj-Large-Precategory l1} {Y : obj-Large-Precategory l2}
      {Z : obj-Large-Precategory l3} →
      type-Set (hom-Large-Precategory Y Z) →
      type-Set (hom-Large-Precategory X Y) →
      type-Set (hom-Large-Precategory X Z)

    id-hom-Large-Precategory :
      {l1 : Level} {X : obj-Large-Precategory l1} →
      type-Set (hom-Large-Precategory X X)

    associative-comp-hom-Large-Precategory :
      {l1 l2 l3 l4 : Level}
      {X : obj-Large-Precategory l1} {Y : obj-Large-Precategory l2}
      {Z : obj-Large-Precategory l3} {W : obj-Large-Precategory l4} →
      (h : type-Set (hom-Large-Precategory Z W))
      (g : type-Set (hom-Large-Precategory Y Z))
      (f : type-Set (hom-Large-Precategory X Y)) →
      ( comp-hom-Large-Precategory (comp-hom-Large-Precategory h g) f) ＝
      ( comp-hom-Large-Precategory h (comp-hom-Large-Precategory g f))

    left-unit-law-comp-hom-Large-Precategory :
      {l1 l2 : Level}
      {X : obj-Large-Precategory l1} {Y : obj-Large-Precategory l2}
      (f : type-Set (hom-Large-Precategory X Y)) →
      ( comp-hom-Large-Precategory id-hom-Large-Precategory f) ＝ f

    right-unit-law-comp-hom-Large-Precategory :
      {l1 l2 : Level}
      {X : obj-Large-Precategory l1} {Y : obj-Large-Precategory l2}
      (f : type-Set (hom-Large-Precategory X Y)) →
      ( comp-hom-Large-Precategory f id-hom-Large-Precategory) ＝ f

open Large-Precategory public
```

```agda
module _
  {α : Level → Level}
  {β : Level → Level → Level}
  (C : Large-Precategory α β)
  where

  type-hom-Large-Precategory :
    {l1 l2 : Level}
    (X : obj-Large-Precategory C l1)
    (Y : obj-Large-Precategory C l2) →
    UU (β l1 l2)
  type-hom-Large-Precategory X Y = type-Set (hom-Large-Precategory C X Y)

  is-set-type-hom-Large-Precategory :
    {l1 l2 : Level}
    (X : obj-Large-Precategory C l1)
    (Y : obj-Large-Precategory C l2) →
    is-set (type-hom-Large-Precategory X Y)
  is-set-type-hom-Large-Precategory X Y =
    is-set-type-Set (hom-Large-Precategory C X Y)

  ap-comp-hom-Large-Precategory :
    {l1 l2 l3 : Level}
    {X : obj-Large-Precategory C l1}
    {Y : obj-Large-Precategory C l2}
    {Z : obj-Large-Precategory C l3}
    {g g' : type-hom-Large-Precategory Y Z} (p : g ＝ g')
    {f f' : type-hom-Large-Precategory X Y} (q : f ＝ f') →
    comp-hom-Large-Precategory C g f ＝
    comp-hom-Large-Precategory C g' f'
  ap-comp-hom-Large-Precategory = ap-binary (comp-hom-Large-Precategory C)

  comp-hom-Large-Precategory' :
    {l1 l2 l3 : Level}
    {X : obj-Large-Precategory C l1}
    {Y : obj-Large-Precategory C l2}
    {Z : obj-Large-Precategory C l3} →
    type-hom-Large-Precategory X Y →
    type-hom-Large-Precategory Y Z →
    type-hom-Large-Precategory X Z
  comp-hom-Large-Precategory' f g = comp-hom-Large-Precategory C g f
```

### Precategories obtained from large precategories

```agda
module _
  {α : Level → Level} {β : Level → Level → Level}
  (C : Large-Precategory α β)
  where

  precategory-Large-Precategory :
    (l : Level) → Precategory (α l) (β l l)
  pr1 (precategory-Large-Precategory l) =
    obj-Large-Precategory C l
  pr1 (pr2 (precategory-Large-Precategory l)) =
    hom-Large-Precategory C
  pr1 (pr1 (pr2 (pr2 (precategory-Large-Precategory l)))) =
    comp-hom-Large-Precategory C
  pr2 (pr1 (pr2 (pr2 (precategory-Large-Precategory l)))) =
    associative-comp-hom-Large-Precategory C
  pr1 (pr2 (pr2 (pr2 (precategory-Large-Precategory l)))) x =
    id-hom-Large-Precategory C
  pr1 (pr2 (pr2 (pr2 (pr2 (precategory-Large-Precategory l))))) =
    left-unit-law-comp-hom-Large-Precategory C
  pr2 (pr2 (pr2 (pr2 (pr2 (precategory-Large-Precategory l))))) =
    right-unit-law-comp-hom-Large-Precategory C
```
