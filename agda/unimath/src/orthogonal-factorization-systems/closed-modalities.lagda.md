# The closed modalities

```agda
module orthogonal-factorization-systems.closed-modalities where
```

<details><summary>Imports</summary>

```agda
open import foundation.contractible-maps
open import foundation.contractible-types
open import foundation.dependent-pair-types
open import foundation.equivalences
open import foundation.function-types
open import foundation.functoriality-dependent-pair-types
open import foundation.identity-types
open import foundation.propositions
open import foundation.subuniverses
open import foundation.type-arithmetic-dependent-pair-types
open import foundation.universe-levels

open import orthogonal-factorization-systems.modal-operators
open import orthogonal-factorization-systems.reflective-subuniverses
open import orthogonal-factorization-systems.sigma-closed-reflective-subuniverses

open import synthetic-homotopy-theory.joins-of-types
```

</details>

## Idea

Given any [proposition](foundation.propositions.md) `Q`, the
[join operation](synthetic-homotopy-theory.joins-of-types.md) `_* Q` defines a
[higher modality](orthogonal-factorization-systems.higher-modalities.md). We
call these the **closed modalities**.

## Definition

```agda
operator-closed-modality :
  (l : Level) {lQ : Level} (Q : Prop lQ) → operator-modality l (l ⊔ lQ)
operator-closed-modality l Q A = A * type-Prop Q

unit-closed-modality :
  {l lQ : Level} (Q : Prop lQ) → unit-modality (operator-closed-modality l Q)
unit-closed-modality Q {A} = inl-join A (type-Prop Q)

is-closed-modal :
  {l lQ : Level} (Q : Prop lQ) → UU l → Prop (l ⊔ lQ)
pr1 (is-closed-modal Q B) = type-Prop Q → is-contr B
pr2 (is-closed-modal Q B) = is-prop-function-type is-property-is-contr
```

## Properties

### The closed modalities define Σ-closed reflective subuniverses

```agda
module _
  {l lQ : Level} (Q : Prop lQ)
  where

  is-reflective-subuniverse-closed-modality :
    is-reflective-subuniverse {l ⊔ lQ} (is-closed-modal Q)
  pr1 is-reflective-subuniverse-closed-modality =
    operator-closed-modality (l ⊔ lQ) Q
  pr1 (pr2 is-reflective-subuniverse-closed-modality) =
    unit-closed-modality Q
  pr1 (pr2 (pr2 is-reflective-subuniverse-closed-modality)) A q =
    right-zero-law-join-is-contr
      ( A)
      ( type-Prop Q)
      ( is-proof-irrelevant-is-prop (is-prop-type-Prop Q) q)
  pr2 (pr2 (pr2 is-reflective-subuniverse-closed-modality)) B A is-modal-B =
    is-equiv-is-contr-map
      ( λ f →
        is-contr-equiv
          ( Σ (A → B) (_＝ f))
          ( equiv-Σ
            ( _＝ f)
            ( right-unit-law-Σ-is-contr
              ( λ f' →
                is-contr-Σ
                  ( is-contr-Π is-modal-B)
                  ( center ∘ is-modal-B)
                  ( is-contr-Π
                    ( λ (a , q) →
                      is-prop-is-contr
                        ( is-modal-B q)
                        ( f' a)
                        ( center (is-modal-B q))))) ∘e
              ( equiv-up-join A (type-Prop Q) B))
            ( λ _ → id-equiv))
          ( is-contr-total-path' f))

  reflective-subuniverse-closed-modality :
    reflective-subuniverse (l ⊔ lQ) (l ⊔ lQ)
  pr1 reflective-subuniverse-closed-modality =
    is-closed-modal Q
  pr2 reflective-subuniverse-closed-modality =
    is-reflective-subuniverse-closed-modality

  is-closed-under-Σ-reflective-subuniverse-closed-modality :
    is-closed-under-Σ-reflective-subuniverse
      ( reflective-subuniverse-closed-modality)
  is-closed-under-Σ-reflective-subuniverse-closed-modality A B q =
    is-contr-Σ
      ( pr2 A q)
      ( center (pr2 A q))
      ( pr2 (B (center (pr2 A q))) q)

  closed-under-Σ-reflective-subuniverse-closed-modality :
    closed-under-Σ-reflective-subuniverse (l ⊔ lQ) (l ⊔ lQ)
  pr1 closed-under-Σ-reflective-subuniverse-closed-modality =
    reflective-subuniverse-closed-modality
  pr2 closed-under-Σ-reflective-subuniverse-closed-modality =
    is-closed-under-Σ-reflective-subuniverse-closed-modality
```

## References

- Egbert Rijke, Michael Shulman, Bas Spitters, _Modalities in homotopy type
  theory_, Logical Methods in Computer Science, Volume 16, Issue 1, 2020
  ([arXiv:1706.07526](https://arxiv.org/abs/1706.07526),
  [doi:10.23638](https://doi.org/10.23638/LMCS-16%281%3A2%292020))
