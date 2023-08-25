# Normal closures of subgroups

```agda
module group-theory.normal-closures-subgroups where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.existential-quantification
open import foundation.fibers-of-maps
open import foundation.function-types
open import foundation.identity-types
open import foundation.logical-equivalences
open import foundation.propositional-truncations
open import foundation.subtypes
open import foundation.universe-levels

open import group-theory.conjugation
open import group-theory.groups
open import group-theory.normal-subgroups
open import group-theory.subgroups
open import group-theory.subgroups-generated-by-subsets-groups
open import group-theory.subsets-groups

open import order-theory.galois-connections-large-posets
open import order-theory.order-preserving-maps-large-posets
open import order-theory.order-preserving-maps-large-preorders
```

</details>

## Idea

Consider a [subgroup](group-theory.subgroups.md) `H` of a
[group](group-theory.groups.md) `G`. The **normal closure** `jH` of `G` is the
least [normal subgroup](group-theory.normal-subgroups.md) of `G` that contains
`H`. The normal closure of `H` is the
[subgroup generated by](group-theory.subgroups-generated-by-subsets-groups.md)
all the [conjugates](group-theory.conjugation.md) of elements of `H`.

In other words, the normal closure operation is the lower adjoint in a
[Galois connection](order-theory.galois-connections-large-posets.md) between the
[large poset](order-theory.large-posets.md) of normal subgroups of `G` and
subgroups of `G`. The upper adjoint of this Galois connection is the inclusion
function from normal subgroups to subgroups of `G`.

Note: The normal closure should not be confused with the
[normalizer](group-theory.normalizer-subgroups.md) of a subgroup, or with the
[normal core](group-theory.normal-cores-subgroups.md) of a subgroup.

## Definitions

### The universal property of the normal closure

```agda
module _
  {l1 l2 l3 : Level} (G : Group l1) (H : Subgroup l2 G)
  (N : Normal-Subgroup l3 G)
  where

  is-normal-closure-Subgroup : UUω
  is-normal-closure-Subgroup =
    {l : Level} (M : Normal-Subgroup l G) →
    leq-Normal-Subgroup G N M ↔ leq-Subgroup G H (subgroup-Normal-Subgroup G M)
```

### The construction of the normal closure

```agda
module _
  {l1 l2 : Level} (G : Group l1) (H : Subgroup l2 G)
  where

  generating-subset-normal-closure-Subgroup : subset-Group (l1 ⊔ l2) G
  generating-subset-normal-closure-Subgroup x =
    ∃-Prop
      ( type-Group G)
      ( λ y → fib (conjugation-Group G y ∘ inclusion-Subgroup G H) x)

  contains-subgroup-generating-subset-normal-closure-Subgroup :
    subset-Subgroup G H ⊆ generating-subset-normal-closure-Subgroup
  contains-subgroup-generating-subset-normal-closure-Subgroup x h =
    unit-trunc-Prop
      ( unit-Group G , (x , h) , compute-conjugation-unit-Group G x)

  is-closed-under-conjugation-generating-subset-normal-closure-Subgroup :
    is-closed-under-conjugation-subset-Group G
      generating-subset-normal-closure-Subgroup
  is-closed-under-conjugation-generating-subset-normal-closure-Subgroup x y s =
    apply-universal-property-trunc-Prop s
      ( generating-subset-normal-closure-Subgroup (conjugation-Group G x y))
      ( λ { (z , h , refl) →
            unit-trunc-Prop
              ( mul-Group G x z ,
                h ,
                compute-conjugation-mul-Group G x z (pr1 h))})

  subgroup-normal-closure-Subgroup : Subgroup (l1 ⊔ l2) G
  subgroup-normal-closure-Subgroup =
    subgroup-subset-Group G generating-subset-normal-closure-Subgroup

  subset-normal-closure-Subgroup : subset-Group (l1 ⊔ l2) G
  subset-normal-closure-Subgroup =
    subset-Subgroup G subgroup-normal-closure-Subgroup

  is-in-normal-closure-Subgroup : type-Group G → UU (l1 ⊔ l2)
  is-in-normal-closure-Subgroup =
    is-in-Subgroup G subgroup-normal-closure-Subgroup

  is-closed-under-eq-normal-closure-Subgroup :
    {x y : type-Group G} → is-in-normal-closure-Subgroup x → x ＝ y →
    is-in-normal-closure-Subgroup y
  is-closed-under-eq-normal-closure-Subgroup =
    is-closed-under-eq-Subgroup G subgroup-normal-closure-Subgroup

  is-closed-under-eq-normal-closure-Subgroup' :
    {x y : type-Group G} → is-in-normal-closure-Subgroup y → x ＝ y →
    is-in-normal-closure-Subgroup x
  is-closed-under-eq-normal-closure-Subgroup' =
    is-closed-under-eq-Subgroup' G subgroup-normal-closure-Subgroup

  contains-unit-normal-closure-Subgroup :
    contains-unit-subset-Group G subset-normal-closure-Subgroup
  contains-unit-normal-closure-Subgroup =
    contains-unit-Subgroup G subgroup-normal-closure-Subgroup

  is-closed-under-multiplication-normal-closure-Subgroup :
    is-closed-under-multiplication-subset-Group G subset-normal-closure-Subgroup
  is-closed-under-multiplication-normal-closure-Subgroup =
    is-closed-under-multiplication-Subgroup G subgroup-normal-closure-Subgroup

  is-closed-under-inv-normal-closure-Subgroup :
    is-closed-under-inv-subset-Group G subset-normal-closure-Subgroup
  is-closed-under-inv-normal-closure-Subgroup =
    is-closed-under-inv-Subgroup G subgroup-normal-closure-Subgroup

  contains-generating-subset-normal-closure-Subgroup :
    generating-subset-normal-closure-Subgroup ⊆ subset-normal-closure-Subgroup
  contains-generating-subset-normal-closure-Subgroup =
    contains-subset-subgroup-subset-Group G
      generating-subset-normal-closure-Subgroup

  is-subgroup-generated-by-generating-subset-normal-closure-Subgroup :
    is-subgroup-generated-by-subset-Group G
      generating-subset-normal-closure-Subgroup
      subgroup-normal-closure-Subgroup
      contains-generating-subset-normal-closure-Subgroup
  is-subgroup-generated-by-generating-subset-normal-closure-Subgroup =
    is-subgroup-generated-by-subset-subgroup-subset-Group G
      generating-subset-normal-closure-Subgroup

  is-normal-subgroup-normal-closure-Subgroup :
    is-normal-Subgroup G subgroup-normal-closure-Subgroup
  is-normal-subgroup-normal-closure-Subgroup =
    is-normal-is-closed-under-conjugation-subgroup-subset-Group G
      generating-subset-normal-closure-Subgroup
      is-closed-under-conjugation-generating-subset-normal-closure-Subgroup

  normal-closure-Subgroup : Normal-Subgroup (l1 ⊔ l2) G
  pr1 normal-closure-Subgroup = subgroup-normal-closure-Subgroup
  pr2 normal-closure-Subgroup = is-normal-subgroup-normal-closure-Subgroup

  contains-subgroup-normal-closure-Subgroup :
    leq-Subgroup G H subgroup-normal-closure-Subgroup
  contains-subgroup-normal-closure-Subgroup =
    transitive-leq-subtype
      ( subset-Subgroup G H)
      ( generating-subset-normal-closure-Subgroup)
      ( subset-normal-closure-Subgroup)
      ( contains-subset-subgroup-subset-Group G
        generating-subset-normal-closure-Subgroup)
      ( contains-subgroup-generating-subset-normal-closure-Subgroup)

  forward-implication-is-normal-closure-normal-closure-Subgroup :
    {l : Level} (N : Normal-Subgroup l G) →
    leq-Normal-Subgroup G normal-closure-Subgroup N →
    leq-Subgroup G H (subgroup-Normal-Subgroup G N)
  forward-implication-is-normal-closure-normal-closure-Subgroup N u =
    transitive-leq-subtype
      ( subset-Subgroup G H)
      ( subset-normal-closure-Subgroup)
      ( subset-Normal-Subgroup G N)
      ( u)
      ( contains-subgroup-normal-closure-Subgroup)

  contains-generating-subset-normal-closure-Normal-Subgroup :
    {l : Level} (N : Normal-Subgroup l G) →
    leq-Subgroup G H (subgroup-Normal-Subgroup G N) →
    generating-subset-normal-closure-Subgroup ⊆ subset-Normal-Subgroup G N
  contains-generating-subset-normal-closure-Normal-Subgroup N u x g =
    apply-universal-property-trunc-Prop g
      ( subset-Normal-Subgroup G N x)
      ( λ { (z , (y , h) , refl) →
            is-normal-Normal-Subgroup G N z y (u y h)})

  backward-implication-is-normal-closure-normal-closure-Subgroup :
    {l : Level} (N : Normal-Subgroup l G) →
    leq-Subgroup G H (subgroup-Normal-Subgroup G N) →
    leq-Normal-Subgroup G normal-closure-Subgroup N
  backward-implication-is-normal-closure-normal-closure-Subgroup N u =
    is-subgroup-generated-by-generating-subset-normal-closure-Subgroup
      ( subgroup-Normal-Subgroup G N)
      ( contains-generating-subset-normal-closure-Normal-Subgroup N u)

  is-normal-closure-normal-closure-Subgroup :
    is-normal-closure-Subgroup G H normal-closure-Subgroup
  pr1 (is-normal-closure-normal-closure-Subgroup N) =
    forward-implication-is-normal-closure-normal-closure-Subgroup N
  pr2 (is-normal-closure-normal-closure-Subgroup N) =
    backward-implication-is-normal-closure-normal-closure-Subgroup N
```

### The normal closure Galois connection

```agda
module _
  {l1 : Level} (G : Group l1)
  where

  preserves-order-normal-closure-Subgroup :
    {l2 l3 : Level} (H : Subgroup l2 G) (K : Subgroup l3 G) →
    leq-Subgroup G H K →
    leq-Normal-Subgroup G
      ( normal-closure-Subgroup G H)
      ( normal-closure-Subgroup G K)
  preserves-order-normal-closure-Subgroup H K u =
    backward-implication-is-normal-closure-normal-closure-Subgroup G H
      ( normal-closure-Subgroup G K)
      ( transitive-leq-subtype
        ( subset-Subgroup G H)
        ( subset-Subgroup G K)
        ( subset-normal-closure-Subgroup G K)
        ( contains-subgroup-normal-closure-Subgroup G K)
        ( u))

  normal-closure-subgroup-hom-Large-Poset :
    hom-Large-Poset
      ( λ l2 → l1 ⊔ l2)
      ( Subgroup-Large-Poset G)
      ( Normal-Subgroup-Large-Poset G)
  normal-closure-subgroup-hom-Large-Poset =
    make-hom-Large-Preorder
      ( normal-closure-Subgroup G)
      ( preserves-order-normal-closure-Subgroup)

  normal-closure-Galois-Connection :
    galois-connection-Large-Poset
      ( λ l2 → l1 ⊔ l2)
      ( id)
      ( Subgroup-Large-Poset G)
      ( Normal-Subgroup-Large-Poset G)
  normal-closure-Galois-Connection =
    make-galois-connection-Large-Poset
      ( normal-closure-subgroup-hom-Large-Poset)
      ( subgroup-normal-subgroup-hom-Large-Poset G)
      ( λ H N → is-normal-closure-normal-closure-Subgroup G H N)
```

## See also

- [Centralizer subgroups](group-theory.centralizer-subgroups.md)
- [Normal cores of subgroups](group-theory.normal-cores-subgroups.md)
- [Normalizers of subgroups](group-theory.normalizer-subgroups.md)