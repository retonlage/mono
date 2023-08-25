# Functoriality of dependent pair types

```agda
module foundation-core.functoriality-dependent-pair-types where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.universe-levels

open import foundation-core.contractible-maps
open import foundation-core.contractible-types
open import foundation-core.equality-dependent-pair-types
open import foundation-core.equivalences
open import foundation-core.fibers-of-maps
open import foundation-core.function-types
open import foundation-core.homotopies
open import foundation-core.identity-types
open import foundation-core.transport
```

</details>

## Idea

Any map `f : A → B` and any family of maps `g : (a : A) → C a → D (f a)`
together induce a map `map-Σ D f g : Σ A C → Σ B D`. Specific instances of this
construction are also very useful: if we take `f` to be the identity map, then
we see that any family of maps `g : (a : A) → C a → D a` induces a map on total
spaces `Σ A C → Σ A D`; if we take `g` to be the family of identity maps, then
we see that for any family `D` over `B`, any map `f : A → B` induces a map
`Σ A (D ∘ f) → Σ B D`.

## Definitions

### The induced map on total spaces

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : A → UU l2} {C : A → UU l3}
  (f : (x : A) → B x → C x)
  where
```

Any family of maps induces a map on the total spaces.

```agda
  tot : Σ A B → Σ A C
  pr1 (tot t) = pr1 t
  pr2 (tot t) = f (pr1 t) (pr2 t)
```

### Any map `f : A → B` induces a map `Σ A (C ∘ f) → Σ B C`

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} (f : A → B) (C : B → UU l3)
  where

  map-Σ-map-base : Σ A (λ x → C (f x)) → Σ B C
  pr1 (map-Σ-map-base s) = f (pr1 s)
  pr2 (map-Σ-map-base s) = pr2 s
```

### The functorial action of dependent pair types, and its defining homotopy

```agda
module _
  {l1 l2 l3 l4 : Level} {A : UU l1} {B : UU l2} {C : A → UU l3}
  (D : B → UU l4)
  where

  map-Σ :
    (f : A → B) (g : (x : A) → C x → D (f x)) → Σ A C → Σ B D
  pr1 (map-Σ f g t) = f (pr1 t)
  pr2 (map-Σ f g t) = g (pr1 t) (pr2 t)

  triangle-map-Σ :
    (f : A → B) (g : (x : A) → C x → D (f x)) →
    (map-Σ f g) ~ (map-Σ-map-base f D ∘ tot g)
  triangle-map-Σ f g t = refl
```

## Properties

### The map `map-Σ` preserves homotopies

```agda
module _
  {l1 l2 l3 l4 : Level} {A : UU l1} {B : UU l2} {C : A → UU l3}
  (D : B → UU l4)
  where

  htpy-map-Σ :
    {f f' : A → B} (H : f ~ f')
    (g : (x : A) → C x → D (f x)) {g' : (x : A) → C x → D (f' x)}
    (K : (x : A) → ((tr D (H x)) ∘ (g x)) ~ (g' x)) →
    (map-Σ D f g) ~ (map-Σ D f' g')
  htpy-map-Σ H g K t = eq-pair-Σ' (pair (H (pr1 t)) (K (pr1 t) (pr2 t)))
```

### The map `tot` preserves homotopies

```agda
tot-htpy :
  {l1 l2 l3 : Level} {A : UU l1} {B : A → UU l2} {C : A → UU l3}
  {f g : (x : A) → B x → C x} → (H : (x : A) → f x ~ g x) → tot f ~ tot g
tot-htpy H (pair x y) = eq-pair-Σ refl (H x y)
```

### The map `tot` preserves identity maps

```agda
tot-id :
  {l1 l2 : Level} {A : UU l1} (B : A → UU l2) →
  (tot (λ x (y : B x) → y)) ~ id
tot-id B (pair x y) = refl
```

### the map `tot` preserves composition

```agda
preserves-comp-tot :
  {l1 l2 l3 l4 : Level}
  {A : UU l1} {B : A → UU l2} {B' : A → UU l3} {B'' : A → UU l4}
  (f : (x : A) → B x → B' x) (g : (x : A) → B' x → B'' x) →
  tot (λ x → (g x) ∘ (f x)) ~ ((tot g) ∘ (tot f))
preserves-comp-tot f g (pair x y) = refl
```

### The fibers of `tot`

We show that for any family of maps, the fiber of the induced map on total
spaces are equivalent to the fibers of the maps in the family.

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : A → UU l2} {C : A → UU l3}
  (f : (x : A) → B x → C x)
  where

  map-compute-fib-tot : (t : Σ A C) → fib (tot f) t → fib (f (pr1 t)) (pr2 t)
  pr1 (map-compute-fib-tot .(tot f (pair x y)) (pair (pair x y) refl)) = y
  pr2 (map-compute-fib-tot .(tot f (pair x y)) (pair (pair x y) refl)) = refl

  map-inv-compute-fib-tot :
    (t : Σ A C) → fib (f (pr1 t)) (pr2 t) → fib (tot f) t
  pr1 (pr1 (map-inv-compute-fib-tot (pair a .(f a y)) (pair y refl))) = a
  pr2 (pr1 (map-inv-compute-fib-tot (pair a .(f a y)) (pair y refl))) = y
  pr2 (map-inv-compute-fib-tot (pair a .(f a y)) (pair y refl)) = refl

  is-section-map-inv-compute-fib-tot :
    (t : Σ A C) → (map-compute-fib-tot t ∘ map-inv-compute-fib-tot t) ~ id
  is-section-map-inv-compute-fib-tot (pair x .(f x y)) (pair y refl) = refl

  is-retraction-map-inv-compute-fib-tot :
    (t : Σ A C) → (map-inv-compute-fib-tot t ∘ map-compute-fib-tot t) ~ id
  is-retraction-map-inv-compute-fib-tot ._ (pair (pair x y) refl) =
    refl

  abstract
    is-equiv-map-compute-fib-tot :
      (t : Σ A C) → is-equiv (map-compute-fib-tot t)
    is-equiv-map-compute-fib-tot t =
      is-equiv-has-inverse
        ( map-inv-compute-fib-tot t)
        ( is-section-map-inv-compute-fib-tot t)
        ( is-retraction-map-inv-compute-fib-tot t)

  compute-fib-tot : (t : Σ A C) → fib (tot f) t ≃ fib (f (pr1 t)) (pr2 t)
  pr1 (compute-fib-tot t) = map-compute-fib-tot t
  pr2 (compute-fib-tot t) = is-equiv-map-compute-fib-tot t

  abstract
    is-equiv-map-inv-compute-fib-tot :
      (t : Σ A C) → is-equiv (map-inv-compute-fib-tot t)
    is-equiv-map-inv-compute-fib-tot t =
      is-equiv-has-inverse
        ( map-compute-fib-tot t)
        ( is-retraction-map-inv-compute-fib-tot t)
        ( is-section-map-inv-compute-fib-tot t)

  inv-compute-fib-tot : (t : Σ A C) → fib (f (pr1 t)) (pr2 t) ≃ fib (tot f) t
  pr1 (inv-compute-fib-tot t) = map-inv-compute-fib-tot t
  pr2 (inv-compute-fib-tot t) = is-equiv-map-inv-compute-fib-tot t
```

### A family of maps `f` is a family of equivalences if and only if `tot f` is an equivalence

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : A → UU l2} {C : A → UU l3}
  {f : (x : A) → B x → C x}
  where

  abstract
    is-equiv-tot-is-fiberwise-equiv : is-fiberwise-equiv f → is-equiv (tot f)
    is-equiv-tot-is-fiberwise-equiv H =
      is-equiv-is-contr-map
        ( λ t →
          is-contr-is-equiv
            ( fib (f (pr1 t)) (pr2 t))
            ( map-compute-fib-tot f t)
            ( is-equiv-map-compute-fib-tot f t)
            ( is-contr-map-is-equiv (H (pr1 t)) (pr2 t)))

  abstract
    is-fiberwise-equiv-is-equiv-tot : is-equiv (tot f) → is-fiberwise-equiv f
    is-fiberwise-equiv-is-equiv-tot is-equiv-tot-f x =
      is-equiv-is-contr-map
        ( λ z →
          is-contr-is-equiv'
            ( fib (tot f) (pair x z))
            ( map-compute-fib-tot f (pair x z))
            ( is-equiv-map-compute-fib-tot f (pair x z))
            ( is-contr-map-is-equiv is-equiv-tot-f (pair x z)))
```

### The action of `tot` on equivalences

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : A → UU l2} {C : A → UU l3}
  where

  equiv-tot : ((x : A) → B x ≃ C x) → (Σ A B) ≃ (Σ A C)
  pr1 (equiv-tot e) = tot (λ x → map-equiv (e x))
  pr2 (equiv-tot e) =
    is-equiv-tot-is-fiberwise-equiv (λ x → is-equiv-map-equiv (e x))
```

### The fibers of `map-Σ-map-base`

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} (f : A → B) (C : B → UU l3)
  where

  fib-map-Σ-map-base-fib :
    (t : Σ B C) → fib f (pr1 t) → fib (map-Σ-map-base f C) t
  pr1 (pr1 (fib-map-Σ-map-base-fib (pair .(f x) z) (pair x refl))) = x
  pr2 (pr1 (fib-map-Σ-map-base-fib (pair .(f x) z) (pair x refl))) = z
  pr2 (fib-map-Σ-map-base-fib (pair .(f x) z) (pair x refl)) = refl

  fib-fib-map-Σ-map-base :
    (t : Σ B C) → fib (map-Σ-map-base f C) t → fib f (pr1 t)
  pr1
    ( fib-fib-map-Σ-map-base
      .(map-Σ-map-base f C (pair x z)) (pair (pair x z) refl)) = x
  pr2
    ( fib-fib-map-Σ-map-base
      .(map-Σ-map-base f C (pair x z)) (pair (pair x z) refl)) = refl

  is-section-fib-fib-map-Σ-map-base :
    (t : Σ B C) → (fib-map-Σ-map-base-fib t ∘ fib-fib-map-Σ-map-base t) ~ id
  is-section-fib-fib-map-Σ-map-base .(pair (f x) z) (pair (pair x z) refl) =
    refl

  is-retraction-fib-fib-map-Σ-map-base :
    (t : Σ B C) → (fib-fib-map-Σ-map-base t ∘ fib-map-Σ-map-base-fib t) ~ id
  is-retraction-fib-fib-map-Σ-map-base (pair .(f x) z) (pair x refl) = refl

  abstract
    is-equiv-fib-map-Σ-map-base-fib :
      (t : Σ B C) → is-equiv (fib-map-Σ-map-base-fib t)
    is-equiv-fib-map-Σ-map-base-fib t =
      is-equiv-has-inverse
        ( fib-fib-map-Σ-map-base t)
        ( is-section-fib-fib-map-Σ-map-base t)
        ( is-retraction-fib-fib-map-Σ-map-base t)

  equiv-fib-map-Σ-map-base-fib :
    (t : Σ B C) → fib f (pr1 t) ≃ fib (map-Σ-map-base f C) t
  pr1 (equiv-fib-map-Σ-map-base-fib t) = fib-map-Σ-map-base-fib t
  pr2 (equiv-fib-map-Σ-map-base-fib t) = is-equiv-fib-map-Σ-map-base-fib t
```

### If `f : A → B` is a contractible map, then so is `map-Σ-map-base f C`

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} (f : A → B) (C : B → UU l3)
  where

  abstract
    is-contr-map-map-Σ-map-base :
      is-contr-map f → is-contr-map (map-Σ-map-base f C)
    is-contr-map-map-Σ-map-base is-contr-f (pair y z) =
      is-contr-equiv'
        ( fib f y)
        ( equiv-fib-map-Σ-map-base-fib f C (pair y z))
        ( is-contr-f y)
```

### If `f : A → B` is an equivalence, then so is `map-Σ-map-base f C`

```agda
module _
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} (f : A → B) (C : B → UU l3)
  where

  abstract
    is-equiv-map-Σ-map-base : is-equiv f → is-equiv (map-Σ-map-base f C)
    is-equiv-map-Σ-map-base is-equiv-f =
      is-equiv-is-contr-map
        ( is-contr-map-map-Σ-map-base f C (is-contr-map-is-equiv is-equiv-f))

equiv-Σ-equiv-base :
  {l1 l2 l3 : Level} {A : UU l1} {B : UU l2} (C : B → UU l3) (e : A ≃ B) →
  Σ A (C ∘ (map-equiv e)) ≃ Σ B C
pr1 (equiv-Σ-equiv-base C (pair f is-equiv-f)) = map-Σ-map-base f C
pr2 (equiv-Σ-equiv-base C (pair f is-equiv-f)) =
  is-equiv-map-Σ-map-base f C is-equiv-f
```

### The functorial action of dependent pair types preserves equivalences

```agda
module _
  {l1 l2 l3 l4 : Level} {A : UU l1} {B : UU l2} {C : A → UU l3}
  (D : B → UU l4)
  where

  abstract
    is-equiv-map-Σ :
      (f : A → B) (g : (x : A) → C x → D (f x)) →
      is-equiv f → is-fiberwise-equiv g → is-equiv (map-Σ D f g)
    is-equiv-map-Σ f g is-equiv-f is-fiberwise-equiv-g =
      is-equiv-comp-htpy
        ( map-Σ D f g)
        ( map-Σ-map-base f D)
        ( tot g)
        ( triangle-map-Σ D f g)
        ( is-equiv-tot-is-fiberwise-equiv is-fiberwise-equiv-g)
        ( is-equiv-map-Σ-map-base f D is-equiv-f)

  equiv-Σ :
    (e : A ≃ B) (g : (x : A) → C x ≃ D (map-equiv e x)) → Σ A C ≃ Σ B D
  pr1 (equiv-Σ e g) = map-Σ D (map-equiv e) (λ x → map-equiv (g x))
  pr2 (equiv-Σ e g) =
    is-equiv-map-Σ
      ( map-equiv e)
      ( λ x → map-equiv (g x))
      ( is-equiv-map-equiv e)
      ( λ x → is-equiv-map-equiv (g x))

  abstract
    is-fiberwise-equiv-is-equiv-map-Σ :
      (f : A → B) (g : (x : A) → C x → D (f x)) →
      is-equiv f → is-equiv (map-Σ D f g) → is-fiberwise-equiv g
    is-fiberwise-equiv-is-equiv-map-Σ f g H K =
      is-fiberwise-equiv-is-equiv-tot
        ( is-equiv-right-factor-htpy
          ( map-Σ D f g)
          ( map-Σ-map-base f D)
          ( tot g)
          ( triangle-map-Σ D f g)
          ( is-equiv-map-Σ-map-base f D H)
          ( K))

  map-equiv-Σ :
    (e : A ≃ B) (g : (x : A) → C x ≃ D (map-equiv e x)) → Σ A C → Σ B D
  map-equiv-Σ e g = map-equiv (equiv-Σ e g)
```

### Any commuting triangle induces a map on fibers

```agda
module _
  {l1 l2 l3 : Level} {X : UU l1} {A : UU l2} {B : UU l3}
  (f : A → X) (g : B → X) (h : A → B) (H : f ~ (g ∘ h))
  where

  fib-triangle :
    (x : X) → (fib f x) → (fib g x)
  pr1 (fib-triangle .(f a) (pair a refl)) = h a
  pr2 (fib-triangle .(f a) (pair a refl)) = inv (H a)

  square-tot-fib-triangle :
    (h ∘ (map-equiv-total-fib f)) ~ (map-equiv-total-fib g ∘ tot fib-triangle)
  square-tot-fib-triangle (pair .(f a) (pair a refl)) = refl
```

### In a commuting triangle, the top map is an equivalence if and only if it induces an equivalence on fibers

```agda
module _
  {l1 l2 l3 : Level} {X : UU l1} {A : UU l2} {B : UU l3}
  (f : A → X) (g : B → X) (h : A → B) (H : f ~ (g ∘ h))
  where

  abstract
    is-fiberwise-equiv-is-equiv-triangle :
      (E : is-equiv h) → is-fiberwise-equiv (fib-triangle f g h H)
    is-fiberwise-equiv-is-equiv-triangle E =
      is-fiberwise-equiv-is-equiv-tot
        ( is-equiv-top-is-equiv-bottom-square
          ( map-equiv-total-fib f)
          ( map-equiv-total-fib g)
          ( tot (fib-triangle f g h H))
          ( h)
          ( square-tot-fib-triangle f g h H)
          ( is-equiv-map-equiv-total-fib f)
          ( is-equiv-map-equiv-total-fib g)
          ( E))

  abstract
    is-equiv-triangle-is-fiberwise-equiv :
      is-fiberwise-equiv (fib-triangle f g h H) → is-equiv h
    is-equiv-triangle-is-fiberwise-equiv E =
      is-equiv-bottom-is-equiv-top-square
        ( map-equiv-total-fib f)
        ( map-equiv-total-fib g)
        ( tot (fib-triangle f g h H))
        ( h)
        ( square-tot-fib-triangle f g h H)
        ( is-equiv-map-equiv-total-fib f)
        ( is-equiv-map-equiv-total-fib g)
        ( is-equiv-tot-is-fiberwise-equiv E)
```

### `map-Σ` preserves identity maps

```agda
module _
  {l1 l2 : Level} {A : UU l1} {B : A → UU l2}
  where

  compute-map-Σ-id : map-Σ B id (λ x → id) ~ id
  compute-map-Σ-id = refl-htpy
```

## See also

- Arithmetical laws involving dependent pair types are recorded in
  [`foundation.type-arithmetic-dependent-pair-types`](foundation.type-arithmetic-dependent-pair-types.md).
- Equality proofs in dependent pair types are characterized in
  [`foundation.equality-dependent-pair-types`](foundation.equality-dependent-pair-types.md).
- The universal property of dependent pair types is treated in
  [`foundation.universal-property-dependent-pair-types`](foundation.universal-property-dependent-pair-types.md).

- Functorial properties of cartesian product types are recorded in
  [`foundation.functoriality-cartesian-product-types`](foundation.functoriality-cartesian-product-types.md).
- Functorial properties of dependent product types are recorded in
  [`foundation.functoriality-dependent-function-types`](foundation.functoriality-dependent-function-types.md).
