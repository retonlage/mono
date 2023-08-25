# Groupoids

```agda
module category-theory.groupoids where
```

<details><summary>Imports</summary>

```agda
open import category-theory.categories
open import category-theory.functors-categories
open import category-theory.isomorphisms-categories
open import category-theory.isomorphisms-precategories
open import category-theory.precategories
open import category-theory.pregroupoids

open import foundation.1-types
open import foundation.contractible-types
open import foundation.dependent-pair-types
open import foundation.equivalences
open import foundation.function-types
open import foundation.functoriality-dependent-pair-types
open import foundation.fundamental-theorem-of-identity-types
open import foundation.identity-types
open import foundation.propositions
open import foundation.sets
open import foundation.type-arithmetic-dependent-pair-types
open import foundation.universe-levels
```

</details>

## Idea

A groupoid is a category in which every morphism is an isomorphism.

## Definition

```agda
is-groupoid-Category-Prop :
  {l1 l2 : Level} (C : Category l1 l2) → Prop (l1 ⊔ l2)
is-groupoid-Category-Prop C =
  is-groupoid-Precategory-Prop (precategory-Category C)

is-groupoid-Category :
  {l1 l2 : Level} (C : Category l1 l2) → UU (l1 ⊔ l2)
is-groupoid-Category C =
  is-groupoid-Precategory (precategory-Category C)

Groupoid : (l1 l2 : Level) → UU (lsuc l1 ⊔ lsuc l2)
Groupoid l1 l2 = Σ (Category l1 l2) is-groupoid-Category

module _
  {l1 l2 : Level} (G : Groupoid l1 l2)
  where

  category-Groupoid : Category l1 l2
  category-Groupoid = pr1 G

  precategory-Groupoid : Precategory l1 l2
  precategory-Groupoid = precategory-Category category-Groupoid

  obj-Groupoid : UU l1
  obj-Groupoid = obj-Category category-Groupoid

  hom-Groupoid : obj-Groupoid → obj-Groupoid → Set l2
  hom-Groupoid = hom-Category category-Groupoid

  type-hom-Groupoid : obj-Groupoid → obj-Groupoid → UU l2
  type-hom-Groupoid = type-hom-Category category-Groupoid

  id-hom-Groupoid :
    {x : obj-Groupoid} → type-hom-Groupoid x x
  id-hom-Groupoid = id-hom-Category category-Groupoid

  comp-hom-Groupoid :
    {x y z : obj-Groupoid} → type-hom-Groupoid y z →
    type-hom-Groupoid x y → type-hom-Groupoid x z
  comp-hom-Groupoid = comp-hom-Category category-Groupoid

  associative-comp-hom-Groupoid :
    {x y z w : obj-Groupoid} (h : type-hom-Groupoid z w)
    (g : type-hom-Groupoid y z) (f : type-hom-Groupoid x y) →
    ( comp-hom-Groupoid (comp-hom-Groupoid h g) f) ＝
    ( comp-hom-Groupoid h (comp-hom-Groupoid g f))
  associative-comp-hom-Groupoid =
    associative-comp-hom-Category category-Groupoid

  left-unit-law-comp-hom-Groupoid :
    {x y : obj-Groupoid} (f : type-hom-Groupoid x y) →
    ( comp-hom-Groupoid id-hom-Groupoid f) ＝ f
  left-unit-law-comp-hom-Groupoid =
    left-unit-law-comp-hom-Category category-Groupoid

  right-unit-law-comp-hom-Groupoid :
    {x y : obj-Groupoid} (f : type-hom-Groupoid x y) →
    ( comp-hom-Groupoid f id-hom-Groupoid) ＝ f
  right-unit-law-comp-hom-Groupoid =
    right-unit-law-comp-hom-Category category-Groupoid

  iso-Groupoid : (x y : obj-Groupoid) → UU l2
  iso-Groupoid = iso-Category category-Groupoid

  is-groupoid-Groupoid : is-groupoid-Category category-Groupoid
  is-groupoid-Groupoid = pr2 G
```

## Property

### The type of groupoids with respect to universe levels `l1` and `l2` is equivalent to the type of 1-types in `l1`

#### The groupoid associated to a 1-type

```agda
module _
  {l : Level} (X : 1-Type l)
  where

  obj-groupoid-1-Type : UU l
  obj-groupoid-1-Type = type-1-Type X

  precategory-Groupoid-1-Type : Precategory l l
  pr1 precategory-Groupoid-1-Type = obj-groupoid-1-Type
  pr1 (pr2 precategory-Groupoid-1-Type) = Id-Set X
  pr1 (pr1 (pr2 (pr2 precategory-Groupoid-1-Type))) q p = p ∙ q
  pr2 (pr1 (pr2 (pr2 precategory-Groupoid-1-Type))) r q p = inv (assoc p q r)
  pr1 (pr2 (pr2 (pr2 precategory-Groupoid-1-Type))) x = refl
  pr1 (pr2 (pr2 (pr2 (pr2 precategory-Groupoid-1-Type)))) p = right-unit
  pr2 (pr2 (pr2 (pr2 (pr2 precategory-Groupoid-1-Type)))) p = left-unit

  is-category-groupoid-1-Type :
    is-category-Precategory precategory-Groupoid-1-Type
  is-category-groupoid-1-Type x =
    fundamental-theorem-id
      ( is-contr-equiv'
        ( Σ ( Σ (type-1-Type X) (λ y → x ＝ y))
            ( λ yp →
              Σ ( Σ (pr1 yp ＝ x) (λ q → (q ∙ pr2 yp) ＝ refl))
                ( λ ql → (pr2 yp ∙ pr1 ql) ＝ refl)))
        ( ( equiv-tot
            ( λ y →
              equiv-tot
                ( λ p →
                  associative-Σ
                    ( y ＝ x)
                    ( λ q → (q ∙ p) ＝ refl)
                    ( λ qr → (p ∙ pr1 qr) ＝ refl)))) ∘e
          ( associative-Σ
            ( type-1-Type X)
            ( λ y → x ＝ y)
            ( λ yp →
              Σ ( Σ (pr1 yp ＝ x) (λ q → (q ∙ pr2 yp) ＝ refl))
                ( λ ql → (pr2 yp ∙ pr1 ql) ＝ refl))))
        ( is-contr-Σ
          ( is-contr-total-path x)
          ( x , refl)
          ( is-contr-Σ
            ( is-contr-equiv
              ( Σ (x ＝ x) (λ q → q ＝ refl))
              ( equiv-tot
                ( λ q → equiv-concat (inv right-unit) refl))
              ( is-contr-total-path' refl))
            ( refl , refl)
            ( is-proof-irrelevant-is-prop
              ( is-1-type-type-1-Type X x x refl refl)
              ( refl)))))
      ( iso-eq-Precategory precategory-Groupoid-1-Type x)

  is-groupoid-groupoid-1-Type :
    is-groupoid-Precategory precategory-Groupoid-1-Type
  pr1 (is-groupoid-groupoid-1-Type x y p) = inv p
  pr1 (pr2 (is-groupoid-groupoid-1-Type x y p)) = left-inv p
  pr2 (pr2 (is-groupoid-groupoid-1-Type x y p)) = right-inv p

  groupoid-1-Type : Groupoid l l
  pr1 (pr1 groupoid-1-Type) = precategory-Groupoid-1-Type
  pr2 (pr1 groupoid-1-Type) = is-category-groupoid-1-Type
  pr2 groupoid-1-Type = is-groupoid-groupoid-1-Type
```

#### The 1-type associated to a groupoid

```agda
module _
  {l1 l2 : Level} (G : Groupoid l1 l2)
  where

  1-type-Groupoid : 1-Type l1
  1-type-Groupoid = obj-Category-1-Type (category-Groupoid G)
```

#### The groupoid obtained from the 1-type induced by a groupoid `G` is `G` itself

```agda
module _
  {l1 l2 : Level} (G : Groupoid l1 l2)
  where

  functor-equiv-groupoid-1-type-Groupoid :
    functor-Category
      ( category-Groupoid (groupoid-1-Type (1-type-Groupoid G)))
      ( category-Groupoid G)
  pr1 functor-equiv-groupoid-1-type-Groupoid = id
  pr1 (pr2 functor-equiv-groupoid-1-type-Groupoid) {x} {.x} refl =
    id-hom-Groupoid G
  pr1 (pr2 (pr2 functor-equiv-groupoid-1-type-Groupoid)) {x} refl refl =
    inv (right-unit-law-comp-hom-Groupoid G (id-hom-Groupoid G))
  pr2 (pr2 (pr2 functor-equiv-groupoid-1-type-Groupoid)) x = refl
```

#### The 1-type obtained from the groupoid induced by a 1-type `X` is `X` itself

```agda
module _
  {l : Level} (X : 1-Type l)
  where

  equiv-1-type-groupoid-1-Type :
    type-equiv-1-Type (1-type-Groupoid (groupoid-1-Type X)) X
  equiv-1-type-groupoid-1-Type = id-equiv
```
