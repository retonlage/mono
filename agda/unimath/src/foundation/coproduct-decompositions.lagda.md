# Coproduct decompositions

```agda
module foundation.coproduct-decompositions where
```

<details><summary>Imports</summary>

```agda
open import foundation.action-on-identifications-functions
open import foundation.coproduct-decompositions-subuniverse
open import foundation.dependent-pair-types
open import foundation.equivalence-extensionality
open import foundation.equivalences
open import foundation.function-extensionality
open import foundation.functoriality-coproduct-types
open import foundation.fundamental-theorem-of-identity-types
open import foundation.identity-types
open import foundation.structure-identity-principle
open import foundation.type-arithmetic-coproduct-types
open import foundation.type-arithmetic-dependent-pair-types
open import foundation.type-arithmetic-empty-type
open import foundation.unit-type
open import foundation.univalence
open import foundation.universe-levels

open import foundation-core.contractible-types
open import foundation-core.coproduct-types
open import foundation-core.equality-dependent-pair-types
open import foundation-core.fibers-of-maps
open import foundation-core.function-types
open import foundation-core.functoriality-dependent-pair-types
open import foundation-core.homotopies
open import foundation-core.propositions
open import foundation-core.transport

open import univalent-combinatorics.equality-standard-finite-types
open import univalent-combinatorics.standard-finite-types
```

</details>

## Definitions

### Binary coproduct decomposition

```agda
module _
  {l1 : Level} (l2 l3 : Level) (X : UU l1)
  where

  binary-coproduct-Decomposition : UU (l1 ⊔ lsuc l2 ⊔ lsuc l3)
  binary-coproduct-Decomposition =
    Σ ( UU l2) ( λ A → Σ ( UU l3) ( λ B → (X ≃ (A + B))))

module _
  {l1 l2 l3 : Level} {X : UU l1}
  (d : binary-coproduct-Decomposition l2 l3 X)
  where

  left-summand-binary-coproduct-Decomposition : UU l2
  left-summand-binary-coproduct-Decomposition = pr1 d

  right-summand-binary-coproduct-Decomposition : UU l3
  right-summand-binary-coproduct-Decomposition = pr1 (pr2 d)

  matching-correspondence-binary-coproduct-Decomposition :
    X ≃
    ( left-summand-binary-coproduct-Decomposition +
      right-summand-binary-coproduct-Decomposition)
  matching-correspondence-binary-coproduct-Decomposition = pr2 (pr2 d)
```

## Propositions

### Coproduct decomposition is equivalent to coproduct decomposition of a full subuniverse

```agda
equiv-coproduct-Decomposition-full-subuniverse :
  {l1 : Level} → (X : UU l1) →
  binary-coproduct-Decomposition l1 l1 X ≃
  binary-coproduct-Decomposition-subuniverse (λ _ → unit-Prop) (X , star)
pr1 (equiv-coproduct-Decomposition-full-subuniverse X) d =
  ( left-summand-binary-coproduct-Decomposition d , star) ,
  ( right-summand-binary-coproduct-Decomposition d , star) ,
  ( matching-correspondence-binary-coproduct-Decomposition d)
pr2 (equiv-coproduct-Decomposition-full-subuniverse X) =
  is-equiv-has-inverse
    ( λ d →
      type-left-summand-binary-coproduct-Decomposition-subuniverse
        ( λ _ → unit-Prop)
        ( X , star)
        ( d) ,
      type-right-summand-binary-coproduct-Decomposition-subuniverse
        ( λ _ → unit-Prop)
        ( X , star)
        ( d) ,
      matching-correspondence-binary-coproduct-Decomposition-subuniverse
        ( λ _ → unit-Prop)
        ( X , star)
        ( d))
    ( λ d →
      eq-equiv-binary-coproduct-Decomposition-subuniverse
        ( λ _ → unit-Prop)
        ( X , star)
        ( _)
        ( d)
        ( id-equiv ,
          ( id-equiv ,
            htpy-right-whisk
              ( id-map-coprod _ _)
              ( map-equiv
                ( matching-correspondence-binary-coproduct-Decomposition-subuniverse
                  ( λ _ → unit-Prop)
                  ( X , star)
                  ( d))))))
    ( refl-htpy)
```

### Characterization of equality of binary coproduct Decomposition

```agda
equiv-binary-coproduct-Decomposition :
  {l1 l2 l3 l4 l5 : Level} {A : UU l1}
  (X : binary-coproduct-Decomposition l2 l3 A)
  (Y : binary-coproduct-Decomposition l4 l5 A) →
  UU (l1 ⊔ l2 ⊔ l3 ⊔ l4 ⊔ l5)
equiv-binary-coproduct-Decomposition X Y =
  Σ ( left-summand-binary-coproduct-Decomposition X ≃
      left-summand-binary-coproduct-Decomposition Y)
    ( λ el →
      Σ ( right-summand-binary-coproduct-Decomposition X ≃
          right-summand-binary-coproduct-Decomposition Y)
        ( λ er →
          ( map-coprod (map-equiv el) (map-equiv er) ∘
            map-equiv
              ( matching-correspondence-binary-coproduct-Decomposition X)) ~
          ( map-equiv
            ( matching-correspondence-binary-coproduct-Decomposition Y))))

module _
  {l1 l2 l3 l4 l5 : Level} {A : UU l1}
  (X : binary-coproduct-Decomposition l2 l3 A)
  (Y : binary-coproduct-Decomposition l4 l5 A)
  (e : equiv-binary-coproduct-Decomposition X Y)
  where

  equiv-left-summand-equiv-binary-coproduct-Decomposition :
    left-summand-binary-coproduct-Decomposition X ≃
    left-summand-binary-coproduct-Decomposition Y
  equiv-left-summand-equiv-binary-coproduct-Decomposition = pr1 e

  map-equiv-left-summand-equiv-binary-coproduct-Decomposition :
    left-summand-binary-coproduct-Decomposition X →
    left-summand-binary-coproduct-Decomposition Y
  map-equiv-left-summand-equiv-binary-coproduct-Decomposition =
    map-equiv equiv-left-summand-equiv-binary-coproduct-Decomposition

  equiv-right-summand-equiv-binary-coproduct-Decomposition :
    right-summand-binary-coproduct-Decomposition X ≃
    right-summand-binary-coproduct-Decomposition Y
  equiv-right-summand-equiv-binary-coproduct-Decomposition = pr1 (pr2 e)

  map-equiv-right-summand-equiv-binary-coproduct-Decomposition :
    right-summand-binary-coproduct-Decomposition X →
    right-summand-binary-coproduct-Decomposition Y
  map-equiv-right-summand-equiv-binary-coproduct-Decomposition =
    map-equiv (equiv-right-summand-equiv-binary-coproduct-Decomposition)

module _
  {l1 l2 l3 : Level} {A : UU l1} (X : binary-coproduct-Decomposition l2 l3 A)
  where

  id-equiv-binary-coproduct-Decomposition :
    equiv-binary-coproduct-Decomposition X X
  pr1 id-equiv-binary-coproduct-Decomposition = id-equiv
  pr1 (pr2 id-equiv-binary-coproduct-Decomposition) = id-equiv
  pr2 (pr2 id-equiv-binary-coproduct-Decomposition) x =
    id-map-coprod
      ( left-summand-binary-coproduct-Decomposition X)
      ( right-summand-binary-coproduct-Decomposition X)
      ( map-equiv
        ( matching-correspondence-binary-coproduct-Decomposition X)
        ( x))

  is-contr-total-equiv-binary-coproduct-Decomposition :
    is-contr
      ( Σ ( binary-coproduct-Decomposition l2 l3 A)
          ( equiv-binary-coproduct-Decomposition X))
  is-contr-total-equiv-binary-coproduct-Decomposition =
    is-contr-total-Eq-structure
      ( _)
      ( is-contr-total-equiv ( left-summand-binary-coproduct-Decomposition X))
      ( left-summand-binary-coproduct-Decomposition X , id-equiv)
      ( is-contr-total-Eq-structure
        ( _)
        ( is-contr-total-equiv (right-summand-binary-coproduct-Decomposition X))
        ( right-summand-binary-coproduct-Decomposition X , id-equiv)
        ( is-contr-total-htpy-equiv
          ( equiv-coprod id-equiv id-equiv ∘e
            matching-correspondence-binary-coproduct-Decomposition X)))

  equiv-eq-binary-coproduct-Decomposition :
    (Y : binary-coproduct-Decomposition l2 l3 A) → (X ＝ Y) →
    equiv-binary-coproduct-Decomposition X Y
  equiv-eq-binary-coproduct-Decomposition .X refl =
    id-equiv-binary-coproduct-Decomposition

  is-equiv-equiv-eq-binary-coproduct-Decomposition :
    (Y : binary-coproduct-Decomposition l2 l3 A) →
    is-equiv (equiv-eq-binary-coproduct-Decomposition Y)
  is-equiv-equiv-eq-binary-coproduct-Decomposition =
    fundamental-theorem-id
      is-contr-total-equiv-binary-coproduct-Decomposition
      equiv-eq-binary-coproduct-Decomposition

  extensionality-binary-coproduct-Decomposition :
    (Y : binary-coproduct-Decomposition l2 l3 A) →
    (X ＝ Y) ≃ equiv-binary-coproduct-Decomposition X Y
  pr1 (extensionality-binary-coproduct-Decomposition Y) =
    equiv-eq-binary-coproduct-Decomposition Y
  pr2 (extensionality-binary-coproduct-Decomposition Y) =
    is-equiv-equiv-eq-binary-coproduct-Decomposition Y

  eq-equiv-binary-coproduct-Decomposition :
    (Y : binary-coproduct-Decomposition l2 l3 A) →
    equiv-binary-coproduct-Decomposition X Y → (X ＝ Y)
  eq-equiv-binary-coproduct-Decomposition Y =
    map-inv-equiv (extensionality-binary-coproduct-Decomposition Y)
```

### Equivalence between `X → Fin 2` and `binary-coproduct-Decomposition l1 l1 X`

```agda
module _
  {l1 : Level}
  (X : UU l1)
  where

  module _
    (f : X → Fin 2)
    where

    matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
      (X ≃ ((fib f (inl (inr star))) + (fib f (inr star))))
    matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ =
      ( ( equiv-coprod
          ( left-unit-law-Σ-is-contr ( is-contr-Fin-one-ℕ) ( inr star))
          ( left-unit-law-Σ-is-contr is-contr-unit star)) ∘e
        ( ( right-distributive-Σ-coprod ( Fin 1) unit (λ x → fib f x) ∘e
            ( inv-equiv-total-fib f))))

    compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
      (x : X) →
      (inl (inr star) ＝ f x) →
      Σ ( Σ ( fib f (inl (inr star)))
            ( λ y →
              inl y ＝
              ( map-equiv
                ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
                ( x))))
        ( λ z → pr1 (pr1 z) ＝ x)
    compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
      x p =
      tr
        ( λ a →
          Σ ( Σ ( fib f (inl (inr star)))
              ( λ y →
                inl y ＝
                ( map-coprod
                  ( map-left-unit-law-Σ-is-contr
                    ( is-contr-Fin-one-ℕ)
                    ( inr star))
                  ( map-left-unit-law-Σ-is-contr is-contr-unit star))
                ( map-right-distributive-Σ-coprod
                  ( Fin 1)
                  ( unit)
                  ( λ x → fib f x)
                  ( pr1 a , x , pr2 a))))
            ( λ z → pr1 (pr1 z) ＝ x))
        ( eq-pair-Σ p ( tr-Id-right p (inv p) ∙ left-inv p))
        ( ( ( x , (inv p)) ,
            ( ap
              ( inl)
              ( inv
                ( inv-map-eq-transpose-equiv
                  ( left-unit-law-Σ-is-contr is-contr-Fin-one-ℕ (inr star))
                  ( refl))))) ,
          refl)

    compute-left-inv-matching-correspondence-binary-coporducd-Decomposition-map-into-Fin-Two-ℕ :
      (y : fib f (inl (inr star))) →
      pr1 y ＝
      map-inv-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
        ( inl y)
    compute-left-inv-matching-correspondence-binary-coporducd-Decomposition-map-into-Fin-Two-ℕ
      y =
      map-eq-transpose-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
        ( inv
          ( pr2
            ( pr1
              ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( pr1 y)
                ( inv (pr2 y))))) ∙
          ap
            inl
            ( eq-pair-Σ
              ( pr2
                ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                  ( pr1 y)
                  ( inv (pr2 y))))
              ( eq-is-prop (is-set-Fin 2 _ _))))

    compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
      (x : X) →
      (inr star ＝ f x) →
      Σ ( Σ ( fib f (inr star))
            ( λ y →
              inr y ＝
              ( map-equiv
                ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
                ( x))))
        ( λ z → pr1 (pr1 z) ＝ x)
    compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
      x p =
      tr
        ( λ a →
          Σ ( Σ ( fib f (inr star))
                ( λ y →
                  inr y ＝
                  ( map-coprod
                    ( map-left-unit-law-Σ-is-contr
                      ( is-contr-Fin-one-ℕ)
                      ( inr star))
                    ( map-left-unit-law-Σ-is-contr is-contr-unit star))
                  ( map-right-distributive-Σ-coprod
                    ( Fin 1)
                    ( unit)
                    ( λ x → fib f x)
                    ( pr1 a , x , pr2 a))))
            ( λ z → pr1 (pr1 z) ＝ x))
        ( eq-pair-Σ p ( tr-Id-right p (inv p) ∙ left-inv p))
        ( ( ( x , (inv p)) ,
            ( ap
              ( inr)
              ( inv
                  ( inv-map-eq-transpose-equiv
                    ( left-unit-law-Σ-is-contr is-contr-unit star)
                    ( refl))))) ,
          refl)

    compute-right-inv-matching-correspondence-binary-coporducd-Decomposition-map-into-Fin-Two-ℕ :
      (y : fib f (inr star)) →
      pr1 y ＝
      map-inv-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
        ( inr y)
    compute-right-inv-matching-correspondence-binary-coporducd-Decomposition-map-into-Fin-Two-ℕ
      y =
      map-eq-transpose-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
        ( inv
          ( pr2
            ( pr1
              ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( pr1 y)
                ( inv (pr2 y))))) ∙
          ap
            inr
            ( eq-pair-Σ
              ( pr2
                ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                  ( pr1 y)
                  ( inv (pr2 y))))
              ( eq-is-prop (is-set-Fin 2 _ _))))

    map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
      binary-coproduct-Decomposition l1 l1 X
    pr1 (map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ) =
      fib f (inl (inr star))
    pr1 (pr2 (map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)) =
      fib f (inr star)
    pr2 (pr2 (map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)) =
      matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ

  map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    ( left-summand-binary-coproduct-Decomposition d +
      right-summand-binary-coproduct-Decomposition d) →
    Fin 2
  map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d (inl _) =
    inl (inr star)
  map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d (inr _) =
    inr star

  map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    X → Fin 2
  map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d x =
    map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
      ( d)
      ( map-equiv
        ( matching-correspondence-binary-coproduct-Decomposition d)
        ( x))

  compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    (t :
      ( left-summand-binary-coproduct-Decomposition d) +
      ( right-summand-binary-coproduct-Decomposition d)) →
    ( inl (inr star) ＝
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
        ( d)
        ( t)) →
    Σ ( left-summand-binary-coproduct-Decomposition d)
      ( λ a → inl a ＝ t)
  compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d (inl y) x =
    y , refl

  compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    (x : X) →
    ( inl (inr star) ＝
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d x) →
    Σ ( left-summand-binary-coproduct-Decomposition d)
      ( λ a →
        inl a ＝
        map-equiv (matching-correspondence-binary-coproduct-Decomposition d) x)
  compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d x p =
    compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
      ( d)
      ( map-equiv (matching-correspondence-binary-coproduct-Decomposition d) x)
      ( p)

  compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    (t :
      ( left-summand-binary-coproduct-Decomposition d) +
      ( right-summand-binary-coproduct-Decomposition d)) →
    ( inr star ＝
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
        ( d)
        ( t)) →
    Σ ( right-summand-binary-coproduct-Decomposition d)
      ( λ a → inr a ＝ t)
  compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d (inr y) x =
    y , refl

  compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    (x : X) →
    ( inr star ＝
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d x) →
    Σ ( right-summand-binary-coproduct-Decomposition d)
      ( λ a →
        inr a ＝
        map-equiv (matching-correspondence-binary-coproduct-Decomposition d) x)
  compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d x p =
    compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
      ( d)
      ( map-equiv (matching-correspondence-binary-coproduct-Decomposition d) x)
      ( p)

  is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper :
    (f : X → Fin 2) →
    (x : X) →
    (v : (inl (inr star) ＝ f x) + (inr star ＝ f x)) →
    ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ f) x ＝
      f x)
  is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    f x (inl y) =
    ( inv
      ( ap
          ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
              ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ f))
          ( pr2
            ( pr1
              ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                f
                x
                y)))) ∙
      y)
  is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    f x (inr y) =
    ( inv
      ( ap
          ( λ p →
            map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
              ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ f)
              ( p))
          ( pr2
            ( pr1
              ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                f
                x
                y)))) ∙
      y)

  is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ ∘
      map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ) ~
    id
  is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    f =
    eq-htpy
      ( λ x →
        is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
          ( f)
          ( x)
          ( map-coprod
            ( map-left-unit-law-Σ-is-contr is-contr-Fin-one-ℕ ( inr star))
            ( map-left-unit-law-Σ-is-contr is-contr-unit star)
            ( map-right-distributive-Σ-coprod
              ( Fin 1)
              ( unit)
              ( λ y → y ＝ f x)
              ( f x , refl))))

  equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    left-summand-binary-coproduct-Decomposition
      ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d)) ≃
    left-summand-binary-coproduct-Decomposition d
  equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d =
    ( ( right-unit-law-coprod
          ( left-summand-binary-coproduct-Decomposition d)) ∘e
      ( ( equiv-coprod
          ( right-unit-law-Σ-is-contr (λ _ → is-contr-unit) ∘e
            equiv-tot
              ( λ _ → extensionality-Fin 2 (inl (inr star)) (inl (inr star))))
          ( right-absorption-Σ
              (right-summand-binary-coproduct-Decomposition d) ∘e
            equiv-tot
              ( λ _ → extensionality-Fin 2 (inr star) (inl (inr star))))) ∘e
        ( ( right-distributive-Σ-coprod
            ( left-summand-binary-coproduct-Decomposition d)
            ( right-summand-binary-coproduct-Decomposition d)
            ( λ y →
              map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                d y ＝ inl (inr star))) ∘e
          ( equiv-Σ-equiv-base
            ( λ y →
              map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                d y ＝ inl (inr star))
            ( matching-correspondence-binary-coproduct-Decomposition d)))))

  equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (d : binary-coproduct-Decomposition l1 l1 X) →
    right-summand-binary-coproduct-Decomposition
      ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d)) ≃
    right-summand-binary-coproduct-Decomposition d
  equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d =
    ( ( left-unit-law-coprod
        ( right-summand-binary-coproduct-Decomposition d)) ∘e
      ( ( equiv-coprod
          ( right-absorption-Σ
              ( left-summand-binary-coproduct-Decomposition d) ∘e
            equiv-tot
              ( λ _ → extensionality-Fin 2 (inl (inr star)) (inr star)))
          ( right-unit-law-Σ-is-contr (λ _ → is-contr-unit) ∘e
            equiv-tot
              ( λ _ → extensionality-Fin 2 (inr star) (inr star)))) ∘e
        ( ( right-distributive-Σ-coprod
            ( left-summand-binary-coproduct-Decomposition d)
            ( right-summand-binary-coproduct-Decomposition d)
            ( λ y →
              map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                d y ＝ inr star)) ∘e
          ( equiv-Σ-equiv-base
            ( λ y →
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                d y) ＝
              ( inr star))
            ( matching-correspondence-binary-coproduct-Decomposition d)))))

  compute-equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    ( d : binary-coproduct-Decomposition l1 l1 X) →
    ( inl ∘
      ( map-equiv
        ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( d)))) ~
    ( ( map-equiv
        ( matching-correspondence-binary-coproduct-Decomposition d)) ∘ pr1)
  compute-equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d (x , p) =
    ap
      ( λ x →
        inl
        ( map-equiv
            ( ( right-unit-law-coprod
                ( left-summand-binary-coproduct-Decomposition d)) ∘e
              ( ( equiv-coprod
                  ( right-unit-law-Σ-is-contr (λ _ → is-contr-unit) ∘e
                    equiv-tot
                      ( λ _ →
                        extensionality-Fin 2 (inl (inr star)) (inl (inr star))))
                  ( right-absorption-Σ
                      (right-summand-binary-coproduct-Decomposition d) ∘e
                    equiv-tot
                      ( λ _ →
                        extensionality-Fin 2 (inr star) (inl (inr star))))) ∘e
                ( ( right-distributive-Σ-coprod
                    ( left-summand-binary-coproduct-Decomposition d)
                    ( right-summand-binary-coproduct-Decomposition d)
                    ( λ y →
                      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                        d y ＝
                      inl (inr star))))))
            ( x)))
      ( eq-pair-Σ
        {t =
          pair
            ( inl
              ( pr1
                ( compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                    d x (inv p))))
            ( refl)}
          ( inv
            ( pr2
              ( compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d)
                ( x)
                ( inv p))))
          ( eq-is-prop
            ( is-set-Fin 2 _ _))) ∙
    ( pr2
        ( compute-left-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( d)
          ( x)
          ( inv p)))

  compute-equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    ( d : binary-coproduct-Decomposition l1 l1 X) →
    ( inr ∘
      ( map-equiv
        ( equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( d)))) ~
    ( map-equiv
      ( matching-correspondence-binary-coproduct-Decomposition d) ∘ pr1)
  compute-equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d (x , p) =
    ap
      ( λ x →
        inr
          ( map-equiv
            ( ( left-unit-law-coprod
                ( right-summand-binary-coproduct-Decomposition d)) ∘e
              ( ( equiv-coprod
                  ( right-absorption-Σ
                      ( left-summand-binary-coproduct-Decomposition d) ∘e
                    equiv-tot
                      ( λ _ →
                        extensionality-Fin 2 (inl (inr star)) (inr star)))
                  ( right-unit-law-Σ-is-contr (λ _ → is-contr-unit) ∘e
                    equiv-tot
                      ( λ _ → extensionality-Fin 2 (inr star) (inr star)))) ∘e
                ( ( right-distributive-Σ-coprod
                    ( left-summand-binary-coproduct-Decomposition d)
                    ( right-summand-binary-coproduct-Decomposition d)
                    ( λ y →
                      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
                        d y ＝ inr star)))))
            ( x)))
      ( eq-pair-Σ
        { t =
          ( inr
            ( pr1
              ( compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d)
                ( x)
                ( inv p))) ,
            refl)}
        ( inv
          ( pr2
            ( compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( d)
              ( x)
              ( inv p))))
        ( eq-is-prop
          ( is-set-Fin 2 _ _))) ∙
    ( pr2
      ( compute-right-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( d)
        ( x)
        ( inv p)))

  matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper :
    ( d : binary-coproduct-Decomposition l1 l1 X) →
    ( x : X) →
    ( ( inl (inr star) ＝
        ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( d)
          ( x))) +
      ( inr star ＝
        ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( d)
          ( x)))) →
    ( map-coprod
        ( map-equiv
          ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d)))
        ( map-equiv
          ( equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d))) ∘
      map-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d)))
      ( x) ＝
    map-equiv (matching-correspondence-binary-coproduct-Decomposition d) x
  matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d x (inl p) =
    ap
      ( map-coprod
        ( map-equiv
          ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d)))
        ( map-equiv
          ( equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d))))
      ( inv
        ( pr2
          ( pr1
            ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d))
              ( x)
              ( p))))) ∙
    ( compute-equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
      ( d)
      ( pr1
        ( pr1
          ( pr1
            ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d))
              ( x)
              ( p)))) ,
        ( pr2
          ( pr1
            ( pr1
              ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                  ( d))
                ( x)
                ( p)))))) ∙
      ap
        ( map-equiv
          ( matching-correspondence-binary-coproduct-Decomposition d))
        ( pr2
          ( compute-left-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( d))
            ( x)
            ( p))))
  matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
    d x (inr p) =
    ap
      ( map-coprod
        ( map-equiv
          ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d)))
        ( map-equiv
          ( equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d))))
      ( inv
        ( pr2
          ( pr1
            ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d))
              ( x)
              ( p))))) ∙
    ( compute-equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
      ( d)
      ( pr1
        ( pr1
          ( pr1
            ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d))
              ( x)
              ( p)))) ,
        pr2
          ( pr1
            ( pr1
              ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                  ( d))
                ( x)
                ( p))))) ∙
      ap
        ( map-equiv
          ( matching-correspondence-binary-coproduct-Decomposition d))
        ( pr2
            ( compute-right-matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
              ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
                ( d))
              ( x)
              ( p))))

  value-map-into-Fin-Two-ℕ-eq-zero-or-one-helper :
    (y : Fin 2) → (inl (inr star) ＝ y) + (inr star ＝ y)
  value-map-into-Fin-Two-ℕ-eq-zero-or-one-helper (inl x) =
    inl (ap inl (eq-is-contr is-contr-Fin-one-ℕ))
  value-map-into-Fin-Two-ℕ-eq-zero-or-one-helper (inr x) =
    inr (ap inr (eq-is-contr is-contr-unit))

  value-map-into-Fin-Two-ℕ-eq-zero-or-one :
    (f : X → Fin 2) →
    (x : X) →
    (inl (inr star) ＝ f x) + (inr star ＝ f x)
  value-map-into-Fin-Two-ℕ-eq-zero-or-one f x =
    value-map-into-Fin-Two-ℕ-eq-zero-or-one-helper (f x)

  matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    ( d : binary-coproduct-Decomposition l1 l1 X) →
    ( map-coprod
        ( map-equiv
          ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d)))
        ( map-equiv
          ( equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d))) ∘
      map-equiv
        ( matching-correspondence-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
          (map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
            ( d)))) ~
    map-equiv (matching-correspondence-binary-coproduct-Decomposition d)
  matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
    d x =
    matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ-helper
      ( d)
      ( x)
      ( value-map-into-Fin-Two-ℕ-eq-zero-or-one
        ( map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d)
        ( x))

  is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ ∘
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ) ~
    id
  is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ d =
    eq-equiv-binary-coproduct-Decomposition
      ( ( map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ ∘
          map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
        ( d))
      ( d)
      ( equiv-left-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( d) ,
        equiv-right-summand-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( d) ,
        matching-correspondence-htpy-is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
        ( d))

  is-equiv-map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    is-equiv map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
  is-equiv-map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ =
    is-equiv-has-inverse
      map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
      ( is-section-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)
      ( is-retraction-map-inv-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ)

  equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ :
    (X → Fin 2) ≃ binary-coproduct-Decomposition l1 l1 X
  pr1 equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ =
    map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
  pr2 equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ =
    is-equiv-map-equiv-binary-coproduct-Decomposition-map-into-Fin-Two-ℕ
```
