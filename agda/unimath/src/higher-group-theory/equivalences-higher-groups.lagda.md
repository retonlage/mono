# Equivalences of higher groups

```agda
module higher-group-theory.equivalences-higher-groups where
```

<details><summary>Imports</summary>

```agda
open import foundation.0-connected-types
open import foundation.contractible-types
open import foundation.dependent-pair-types
open import foundation.equivalences
open import foundation.function-types
open import foundation.fundamental-theorem-of-identity-types
open import foundation.identity-types
open import foundation.subtype-identity-principle
open import foundation.universe-levels

open import higher-group-theory.higher-groups
open import higher-group-theory.homomorphisms-higher-groups

open import structured-types.pointed-equivalences
open import structured-types.pointed-types
```

</details>

## Definitions

### Equivalences of higher groups

```agda
module _
  {l1 l2 : Level} (G : ∞-Group l1) (H : ∞-Group l2)
  where

  equiv-∞-Group : UU (l1 ⊔ l2)
  equiv-∞-Group =
    classifying-pointed-type-∞-Group G ≃∗ classifying-pointed-type-∞-Group H

  hom-equiv-∞-Group : equiv-∞-Group → hom-∞-Group G H
  hom-equiv-∞-Group =
    pointed-map-pointed-equiv

  map-equiv-∞-Group : equiv-∞-Group → type-∞-Group G → type-∞-Group H
  map-equiv-∞-Group = map-hom-∞-Group G H ∘ hom-equiv-∞-Group
```

### The identity equivalence of higher groups

```agda
id-equiv-∞-Group :
  {l1 : Level} (G : ∞-Group l1) → equiv-∞-Group G G
id-equiv-∞-Group G = id-pointed-equiv
```

### Isomorphisms of ∞-groups

```agda
module _
  {l1 l2 : Level} (G : ∞-Group l1) (H : ∞-Group l2)
  where

  is-iso-hom-∞-Group : hom-∞-Group G H → UU (l1 ⊔ l2)
  is-iso-hom-∞-Group = is-iso-pointed-map
```

## Properties

### The total space of equivalences of higher groups is contractible

```agda
is-contr-total-equiv-∞-Group :
  {l1 : Level} (G : ∞-Group l1) → is-contr (Σ (∞-Group l1) (equiv-∞-Group G))
is-contr-total-equiv-∞-Group G =
  is-contr-total-Eq-subtype
    ( is-contr-total-equiv-Pointed-Type (classifying-pointed-type-∞-Group G))
    ( λ X → is-prop-is-0-connected (type-Pointed-Type X))
    ( classifying-pointed-type-∞-Group G)
    ( id-pointed-equiv)
    ( is-0-connected-classifying-type-∞-Group G)

equiv-eq-∞-Group :
  {l1 : Level} (G H : ∞-Group l1) → (G ＝ H) → equiv-∞-Group G H
equiv-eq-∞-Group G .G refl = id-equiv-∞-Group G

is-equiv-equiv-eq-∞-Group :
  {l1 : Level} (G H : ∞-Group l1) → is-equiv (equiv-eq-∞-Group G H)
is-equiv-equiv-eq-∞-Group G =
  fundamental-theorem-id
    ( is-contr-total-equiv-∞-Group G)
    ( equiv-eq-∞-Group G)

extensionality-∞-Group :
  {l1 : Level} (G H : ∞-Group l1) → (G ＝ H) ≃ equiv-∞-Group G H
pr1 (extensionality-∞-Group G H) = equiv-eq-∞-Group G H
pr2 (extensionality-∞-Group G H) = is-equiv-equiv-eq-∞-Group G H

eq-equiv-∞-Group :
  {l1 : Level} (G H : ∞-Group l1) → equiv-∞-Group G H → G ＝ H
eq-equiv-∞-Group G H = map-inv-equiv (extensionality-∞-Group G H)
```
