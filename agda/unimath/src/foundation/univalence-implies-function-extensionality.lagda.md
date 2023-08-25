# The univalence axiom implies function extensionality

```agda
module foundation.univalence-implies-function-extensionality where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.equivalence-induction
open import foundation.type-arithmetic-dependent-pair-types
open import foundation.universe-levels
open import foundation.weak-function-extensionality

open import foundation-core.contractible-maps
open import foundation-core.contractible-types
open import foundation-core.fibers-of-maps
open import foundation-core.function-extensionality
open import foundation-core.function-types
open import foundation-core.homotopies
open import foundation-core.identity-types
open import foundation-core.transport
```

</details>

## Idea

The [univalence axiom](foundation-core.univalence.md) implies
[function extensionality](foundation-core.function-extensionality.md).

## Theorem

```agda
abstract
  weak-funext-univalence :
    {l : Level} {A : UU l} {B : A → UU l} → WEAK-FUNEXT A B
  weak-funext-univalence {A = A} {B} is-contr-B =
    is-contr-retract-of
      ( fib (postcomp A (pr1 {B = B})) id)
      ( pair
        ( λ f → pair (λ x → pair x (f x)) refl)
        ( pair
          ( λ h x → tr B (htpy-eq (pr2 h) x) (pr2 (pr1 h x)))
          ( refl-htpy)))
      ( is-contr-map-is-equiv
        ( is-equiv-postcomp-univalence A (equiv-pr1 is-contr-B))
        ( id))

abstract
  funext-univalence :
    {l : Level} {A : UU l} {B : A → UU l} (f : (x : A) → B x) → FUNEXT f
  funext-univalence {A = A} {B} f =
    funext-weak-funext (λ A B → weak-funext-univalence) A B f
```
