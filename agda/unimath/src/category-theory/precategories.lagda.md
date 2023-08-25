# Precategories

```agda
module category-theory.precategories where
```

<details><summary>Imports</summary>

```agda
open import foundation.cartesian-product-types
open import foundation.dependent-pair-types
open import foundation.function-extensionality
open import foundation.identity-types
open import foundation.propositions
open import foundation.sets
open import foundation.subtypes
open import foundation.universe-levels
```

</details>

## Idea

A precategory in Homotopy Type Theory consists of:

- a type `A` of objects,
- for each pair of objects `x y : A`, a set of morphisms `hom x y : Set`,
  together with a composition operation `_∘_ : hom y z → hom x y → hom x z` such
  that:
- `(h ∘ g) ∘ f = h ∘ (g ∘ f)` for any morphisms `h : hom z w`, `g : hom y z` and
  `f : hom x y`,
- for each object `x : A` there is a morphism `id_x : hom x x` such that
  `id_x ∘ f = f` and `g ∘ id_x = g` for any morphisms `f : hom x y` and
  `g : hom z x`.

The reason this is called a *pre*category and not a category in Homotopy Type
Theory is that we want to reserve that name for precategories where the
identities between the objects are exactly the isomorphisms.

## Definition

```agda
module _
  {l1 l2 : Level} {A : UU l1} (hom : A → A → Set l2)
  where

  associative-composition-structure-Set : UU (l1 ⊔ l2)
  associative-composition-structure-Set =
    Σ ( {x y z : A} →
        type-Set (hom y z) → type-Set (hom x y) → type-Set (hom x z))
      ( λ μ →
        {x y z w : A} (h : type-Set (hom z w)) (g : type-Set (hom y z))
        (f : type-Set (hom x y)) → μ (μ h g) f ＝ μ h (μ g f))

  is-unital-composition-structure-Set :
    associative-composition-structure-Set → UU (l1 ⊔ l2)
  is-unital-composition-structure-Set μ =
    Σ ( (x : A) → type-Set (hom x x))
      ( λ e →
        ( {x y : A} (f : type-Set (hom x y)) → pr1 μ (e y) f ＝ f) ×
        ( {x y : A} (f : type-Set (hom x y)) → pr1 μ f (e x) ＝ f))

Precategory :
  (l1 l2 : Level) → UU (lsuc l1 ⊔ lsuc l2)
Precategory l1 l2 =
  Σ ( UU l1)
    ( λ A →
      Σ ( A → A → Set l2)
        ( λ hom →
          Σ ( associative-composition-structure-Set hom)
            ( is-unital-composition-structure-Set hom)))

module _
  {l1 l2 : Level} (C : Precategory l1 l2)
  where

  obj-Precategory : UU l1
  obj-Precategory = pr1 C

  hom-Precategory : (x y : obj-Precategory) → Set l2
  hom-Precategory = pr1 (pr2 C)

  type-hom-Precategory : (x y : obj-Precategory) → UU l2
  type-hom-Precategory x y = type-Set (hom-Precategory x y)

  is-set-type-hom-Precategory :
    (x y : obj-Precategory) → is-set (type-hom-Precategory x y)
  is-set-type-hom-Precategory x y = is-set-type-Set (hom-Precategory x y)

  associative-composition-Precategory :
    associative-composition-structure-Set hom-Precategory
  associative-composition-Precategory = pr1 (pr2 (pr2 C))

  comp-hom-Precategory :
    {x y z : obj-Precategory} →
    type-hom-Precategory y z → type-hom-Precategory x y →
    type-hom-Precategory x z
  comp-hom-Precategory = pr1 associative-composition-Precategory

  comp-hom-Precategory' :
    {x y z : obj-Precategory} →
    type-hom-Precategory x y → type-hom-Precategory y z →
    type-hom-Precategory x z
  comp-hom-Precategory' f g = comp-hom-Precategory g f

  precomp-hom-Precategory :
    {x y : obj-Precategory} (f : type-hom-Precategory x y)
    (z : obj-Precategory) → type-hom-Precategory y z → type-hom-Precategory x z
  precomp-hom-Precategory f z g = comp-hom-Precategory g f

  postcomp-hom-Precategory :
    {x y : obj-Precategory} (f : type-hom-Precategory x y)
    (z : obj-Precategory) → type-hom-Precategory z x → type-hom-Precategory z y
  postcomp-hom-Precategory f z = comp-hom-Precategory f

  associative-comp-hom-Precategory :
    {x y z w : obj-Precategory} (h : type-hom-Precategory z w)
    (g : type-hom-Precategory y z) (f : type-hom-Precategory x y) →
    ( comp-hom-Precategory (comp-hom-Precategory h g) f) ＝
    ( comp-hom-Precategory h (comp-hom-Precategory g f))
  associative-comp-hom-Precategory = pr2 associative-composition-Precategory

  is-unital-Precategory :
    is-unital-composition-structure-Set
      hom-Precategory
      associative-composition-Precategory
  is-unital-Precategory = pr2 (pr2 (pr2 C))

  id-hom-Precategory : {x : obj-Precategory} → type-hom-Precategory x x
  id-hom-Precategory {x} = pr1 is-unital-Precategory x

  left-unit-law-comp-hom-Precategory :
    {x y : obj-Precategory} (f : type-hom-Precategory x y) →
    comp-hom-Precategory id-hom-Precategory f ＝ f
  left-unit-law-comp-hom-Precategory = pr1 (pr2 is-unital-Precategory)

  right-unit-law-comp-hom-Precategory :
    {x y : obj-Precategory} (f : type-hom-Precategory x y) →
    comp-hom-Precategory f id-hom-Precategory ＝ f
  right-unit-law-comp-hom-Precategory = pr2 (pr2 is-unital-Precategory)
```

## Properties

### The property of having identity morphisms is a proposition

Suppose `e e' : (x : A) → hom x x` are both right and left units with regard to
composition. It is enough to show that `e = e'` since the right and left unit
laws are propositions (because all hom-types are sets). By function
extensionality, it is enough to show that `e x = e' x` for all `x : A`. But by
the unit laws we have the following chain of equalities:
`e x = (e' x) ∘ (e x) = e' x.`

```agda
module _
  {l1 l2 : Level} {A : UU l1} (hom : A → A → Set l2)
  where

  abstract
    all-elements-equal-is-unital-composition-structure-Set :
      ( μ : associative-composition-structure-Set hom) →
      all-elements-equal (is-unital-composition-structure-Set hom μ)
    all-elements-equal-is-unital-composition-structure-Set
      ( pair μ associative-μ)
      ( pair e (pair left-unit-law-e right-unit-law-e))
      ( pair e' (pair left-unit-law-e' right-unit-law-e')) =
      eq-type-subtype
        ( λ x →
          prod-Prop
            ( Π-Prop' A
              ( λ a →
                Π-Prop' A
                  ( λ b →
                    Π-Prop
                      ( type-Set (hom a b))
                      ( λ f' →
                        Id-Prop (hom a b) (μ (x b) f') f'))))
            ( Π-Prop' A
              ( λ a →
                Π-Prop' A
                  ( λ b →
                    Π-Prop
                      ( type-Set (hom a b))
                      ( λ f' →
                        Id-Prop (hom a b) (μ f' (x a)) f')))))
        ( eq-htpy
          ( λ x →
            ( inv (left-unit-law-e' (e x))) ∙
            ( right-unit-law-e (e' x))))

    is-prop-is-unital-composition-structure-Set :
      ( μ : associative-composition-structure-Set hom) →
      is-prop (is-unital-composition-structure-Set hom μ)
    is-prop-is-unital-composition-structure-Set μ =
      is-prop-all-elements-equal
        ( all-elements-equal-is-unital-composition-structure-Set μ)
```
