# Cartier's delooping of the sign homomorphism

```agda
{-# OPTIONS --lossy-unification #-}

module finite-group-theory.cartier-delooping-sign-homomorphism where
```

<details><summary>Imports</summary>

```agda
open import elementary-number-theory.addition-natural-numbers
open import elementary-number-theory.natural-numbers

open import finite-group-theory.delooping-sign-homomorphism
open import finite-group-theory.finite-type-groups
open import finite-group-theory.sign-homomorphism
open import finite-group-theory.transpositions

open import foundation.action-on-identifications-functions
open import foundation.contractible-types
open import foundation.dependent-pair-types
open import foundation.equivalence-relations
open import foundation.equivalences
open import foundation.identity-types
open import foundation.mere-equivalences
open import foundation.negation
open import foundation.propositional-truncations
open import foundation.raising-universe-levels
open import foundation.transport
open import foundation.unit-type
open import foundation.univalence-action-on-equivalences
open import foundation.universe-levels

open import group-theory.concrete-groups
open import group-theory.homomorphisms-concrete-groups
open import group-theory.homomorphisms-groups
open import group-theory.isomorphisms-groups
open import group-theory.loop-groups-sets
open import group-theory.symmetric-groups

open import univalent-combinatorics.2-element-decidable-subtypes
open import univalent-combinatorics.orientations-complete-undirected-graph
open import univalent-combinatorics.standard-finite-types
```

</details>

## Idea

We define the delooping of the sign homomorphism by using a method of Cartier.

## Definitions

```agda
module _
  { l : Level}
  where

  not-even-difference-univalent-action-equiv :
    (n : ℕ) (Y : 2-Element-Decidable-Subtype l (raise-Fin l (n +ℕ 2))) →
    ¬ ( sim-Equivalence-Relation
      ( even-difference-orientation-Complete-Undirected-Graph
        ( n +ℕ 2)
        ( raise-Fin l (n +ℕ 2) ,
          unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2))))
      ( orientation-aut-count
        ( n +ℕ 2 , compute-raise l (Fin (n +ℕ 2)))
        ( star)
        ( transposition Y))
      ( map-equiv
        ( univalent-action-equiv
          ( mere-equiv-Prop (Fin (n +ℕ 2)))
          ( orientation-Complete-Undirected-Graph (n +ℕ 2))
          ( raise l (Fin (n +ℕ 2)) ,
            unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2)))
          ( raise l (Fin (n +ℕ 2)) ,
            unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2)))
          ( transposition Y))
        ( orientation-aut-count
          (n +ℕ 2 , compute-raise l (Fin (n +ℕ 2))) star (transposition Y))))
  not-even-difference-univalent-action-equiv n =
    tr
      ( λ f →
        ( Y : 2-Element-Decidable-Subtype l
          ( raise-Fin l (n +ℕ 2))) →
            ¬ ( sim-Equivalence-Relation
              ( even-difference-orientation-Complete-Undirected-Graph
                ( n +ℕ 2)
                ( raise-Fin l (n +ℕ 2) ,
                  unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2))))
              ( orientation-aut-count
                  ( n +ℕ 2 , compute-raise l (Fin (n +ℕ 2)))
                  ( star)
                  ( transposition Y))
              ( map-equiv
                ( f
                  ( raise l (Fin (n +ℕ 2)) ,
                    unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2)))
                  ( raise l (Fin (n +ℕ 2)) ,
                    unit-trunc-Prop (compute-raise-Fin l (n +ℕ 2)))
                  ( transposition Y))
                ( orientation-aut-count
                  ( n +ℕ 2 , compute-raise l (Fin (n +ℕ 2)))
                  ( star)
                  ( transposition Y)))))
      ( ap pr1
        { x =
          orientation-complete-undirected-graph-equiv (n +ℕ 2) ,
          preserves-id-equiv-orientation-complete-undirected-graph-equiv
            ( n +ℕ 2)}
        { y =
          ( univalent-action-equiv
            ( mere-equiv-Prop (Fin (n +ℕ 2)))
            ( orientation-Complete-Undirected-Graph (n +ℕ 2))) ,
          ( preserves-id-equiv-univalent-action-equiv
            ( mere-equiv-Prop (Fin (n +ℕ 2)))
            ( orientation-Complete-Undirected-Graph (n +ℕ 2)))}
        ( eq-is-contr
          ( is-contr-preserves-id-action-equiv
            ( mere-equiv-Prop (Fin (n +ℕ 2)))
            ( orientation-Complete-Undirected-Graph (n +ℕ 2))
            ( is-set-orientation-Complete-Undirected-Graph (n +ℕ 2)))))
      ( not-even-difference-orientation-aut-transposition-count
        (n +ℕ 2 , (compute-raise l (Fin (n +ℕ 2)))) (star))

  cartier-delooping-sign :
    (n : ℕ) → hom-Concrete-Group (UU-Fin-Group l n) (UU-Fin-Group (lsuc l) 2)
  cartier-delooping-sign =
    quotient-delooping-sign
      ( orientation-Complete-Undirected-Graph)
      ( even-difference-orientation-Complete-Undirected-Graph)
      ( λ n _ →
        is-decidable-even-difference-orientation-Complete-Undirected-Graph n)
      ( equiv-fin-2-quotient-sign-equiv-Fin)
      ( λ n →
        orientation-aut-count (n +ℕ 2 , compute-raise l (Fin (n +ℕ 2))) (star))
      ( not-even-difference-univalent-action-equiv)

  eq-cartier-delooping-sign-homomorphism :
    (n : ℕ) →
    Id
      ( comp-hom-Group
        ( symmetric-Group (raise-Fin-Set l (n +ℕ 2)))
        ( loop-group-Set (raise-Fin-Set l (n +ℕ 2)))
        ( abstract-group-Concrete-Group (UU-Fin-Group (lsuc l) 2))
        ( comp-hom-Group
          ( loop-group-Set (raise-Fin-Set l (n +ℕ 2)))
          ( abstract-group-Concrete-Group (UU-Fin-Group l (n +ℕ 2)))
          ( abstract-group-Concrete-Group (UU-Fin-Group (lsuc l) 2))
          ( hom-group-hom-Concrete-Group
            ( UU-Fin-Group l (n +ℕ 2))
            ( UU-Fin-Group (lsuc l) 2)
            ( cartier-delooping-sign (n +ℕ 2)))
          ( hom-inv-iso-Group
            ( abstract-group-Concrete-Group (UU-Fin-Group l (n +ℕ 2)))
            ( loop-group-Set (raise-Fin-Set l (n +ℕ 2)))
            ( iso-loop-group-fin-UU-Fin-Group l (n +ℕ 2))))
        ( hom-inv-symmetric-group-loop-group-Set (raise-Fin-Set l (n +ℕ 2))))
      ( comp-hom-Group
        ( symmetric-Group (raise-Fin-Set l (n +ℕ 2)))
        ( symmetric-Group (Fin-Set (n +ℕ 2)))
        ( abstract-group-Concrete-Group (UU-Fin-Group (lsuc l) 2))
        ( comp-hom-Group
          ( symmetric-Group (Fin-Set (n +ℕ 2)))
          ( symmetric-Group (Fin-Set 2))
          ( abstract-group-Concrete-Group (UU-Fin-Group (lsuc l) 2))
          ( symmetric-abstract-UU-fin-group-quotient-hom
            ( orientation-Complete-Undirected-Graph)
            ( even-difference-orientation-Complete-Undirected-Graph)
            ( λ n _ →
              is-decidable-even-difference-orientation-Complete-Undirected-Graph
                ( n))
            ( equiv-fin-2-quotient-sign-equiv-Fin)
            ( λ n →
              orientation-aut-count
                ( n +ℕ 2 , compute-raise l (Fin (n +ℕ 2)))
                ( star))
            ( not-even-difference-univalent-action-equiv)
            ( n))
          ( sign-homomorphism
            ( n +ℕ 2)
            ( Fin (n +ℕ 2) , unit-trunc-Prop id-equiv)))
        ( hom-inv-symmetric-group-equiv-Set
          ( Fin-Set (n +ℕ 2))
          ( raise-Fin-Set l (n +ℕ 2))
          ( compute-raise l (Fin (n +ℕ 2)))))
  eq-cartier-delooping-sign-homomorphism =
    eq-quotient-delooping-sign-homomorphism
      ( orientation-Complete-Undirected-Graph)
      ( even-difference-orientation-Complete-Undirected-Graph)
      ( λ n _ →
        is-decidable-even-difference-orientation-Complete-Undirected-Graph n)
      ( equiv-fin-2-quotient-sign-equiv-Fin)
      ( λ n →
        orientation-aut-count (n +ℕ 2 , compute-raise l (Fin (n +ℕ 2))) (star))
      ( not-even-difference-univalent-action-equiv)
```

## References

- Mangel É. and Rijke E.
  ["Delooping the sign homomorphism in univalent mathematics"](https://arxiv.org/abs/2301.10011).
