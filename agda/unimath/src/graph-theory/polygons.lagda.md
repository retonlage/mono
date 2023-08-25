# Polygons

```agda
module graph-theory.polygons where
```

<details><summary>Imports</summary>

```agda
open import elementary-number-theory.modular-arithmetic
open import elementary-number-theory.natural-numbers

open import foundation.decidable-equality
open import foundation.dependent-pair-types
open import foundation.fibers-of-maps
open import foundation.functoriality-propositional-truncation
open import foundation.mere-equivalences
open import foundation.sets
open import foundation.universe-levels
open import foundation.unordered-pairs

open import graph-theory.equivalences-undirected-graphs
open import graph-theory.mere-equivalences-undirected-graphs
open import graph-theory.undirected-graphs

open import univalent-combinatorics.finite-types
```

</details>

## Idea

A polygon is an undirected graph that is merely equivalent to a graph with
vertices `ℤ-Mod k` and an edge from each `x ∈ ℤ-Mod k` to `x+1`. This defines
for each `k ∈ ℕ` the type of all `k`-gons. The type of all `k`-gons is a
concrete presentation of the dihedral group `D_k`.

## Definition

### Standard polygons

```agda
vertex-standard-polygon-Undirected-Graph : ℕ → UU lzero
vertex-standard-polygon-Undirected-Graph k = ℤ-Mod k

unordered-pair-vertices-standard-polygon-Undirected-Graph : ℕ → UU (lsuc lzero)
unordered-pair-vertices-standard-polygon-Undirected-Graph k =
  unordered-pair (vertex-standard-polygon-Undirected-Graph k)

edge-standard-polygon-Undirected-Graph :
  (k : ℕ) →
  unordered-pair-vertices-standard-polygon-Undirected-Graph k → UU lzero
edge-standard-polygon-Undirected-Graph k p =
  Σ ( type-unordered-pair p)
    ( λ x →
      fib
        ( element-unordered-pair p)
        ( succ-ℤ-Mod k (element-unordered-pair p x)))

standard-polygon-Undirected-Graph : ℕ → Undirected-Graph lzero lzero
pr1 (standard-polygon-Undirected-Graph k) =
  vertex-standard-polygon-Undirected-Graph k
pr2 (standard-polygon-Undirected-Graph k) =
  edge-standard-polygon-Undirected-Graph k
```

### The type of all polygons with `k` vertices

```agda
Polygon : ℕ → UU (lsuc lzero)
Polygon k =
  Σ ( Undirected-Graph lzero lzero)
    ( mere-equiv-Undirected-Graph (standard-polygon-Undirected-Graph k))

module _
  (k : ℕ) (X : Polygon k)
  where

  undirected-graph-Polygon : Undirected-Graph lzero lzero
  undirected-graph-Polygon = pr1 X

  mere-equiv-Polygon :
    mere-equiv-Undirected-Graph
      ( standard-polygon-Undirected-Graph k)
      ( undirected-graph-Polygon)
  mere-equiv-Polygon = pr2 X

  vertex-Polygon : UU lzero
  vertex-Polygon = vertex-Undirected-Graph undirected-graph-Polygon

  unordered-pair-vertices-Polygon : UU (lsuc lzero)
  unordered-pair-vertices-Polygon = unordered-pair vertex-Polygon

  edge-Polygon : unordered-pair-vertices-Polygon → UU lzero
  edge-Polygon = edge-Undirected-Graph undirected-graph-Polygon

  mere-equiv-vertex-Polygon : mere-equiv (ℤ-Mod k) vertex-Polygon
  mere-equiv-vertex-Polygon =
    map-trunc-Prop
      ( equiv-vertex-equiv-Undirected-Graph
        ( standard-polygon-Undirected-Graph k)
        ( undirected-graph-Polygon))
      ( mere-equiv-Polygon)

  is-finite-vertex-Polygon : is-nonzero-ℕ k → is-finite vertex-Polygon
  is-finite-vertex-Polygon H =
    is-finite-mere-equiv mere-equiv-vertex-Polygon (is-finite-ℤ-Mod H)

  is-set-vertex-Polygon : is-set vertex-Polygon
  is-set-vertex-Polygon =
    is-set-mere-equiv' mere-equiv-vertex-Polygon (is-set-ℤ-Mod k)

  has-decidable-equality-vertex-Polygon : has-decidable-equality vertex-Polygon
  has-decidable-equality-vertex-Polygon =
    has-decidable-equality-mere-equiv'
      ( mere-equiv-vertex-Polygon)
      ( has-decidable-equality-ℤ-Mod k)

{-
  is-prop-edge-Polygon :
    (p : unordered-pair-vertices-Polygon) → is-prop (edge-Polygon p)
  is-prop-edge-Polygon p = {!!}
-}
```

## Properties

### The type of vertices of a polygon is a set

```agda
is-set-vertex-standard-polygon-Undirected-Graph :
  (k : ℕ) → is-set (vertex-standard-polygon-Undirected-Graph k)
is-set-vertex-standard-polygon-Undirected-Graph k = is-set-ℤ-Mod k
```

### Every edge is between distinct points

```agda
{-
module _
  (k : ℕ) (p : unordered-pair-vertices-standard-polygon-Undirected-Graph k)
  where

  is-emb-element-unordered-pair-edge-standard-polygon-Undirected-Graph :
    edge-standard-polygon-Undirected-Graph k p →
    is-emb (element-unordered-pair p)
  is-emb-element-unordered-pair-edge-standard-polygon-Undirected-Graph e =
    is-emb-is-injective
      ( is-set-vertex-standard-polygon-Undirected-Graph k)
      {!!}

  is-prop-edge-standard-polygon-Undirected-Graph :
    is-prop (edge-standard-polygon-Undirected-Graph k p)
  is-prop-edge-standard-polygon-Undirected-Graph =
    {!!}
-}
```

### Every polygon is a simple graph

```agda
{-
is-simple-standard-polygon-Undirected-Graph :
  (k : ℕ) → is-not-one-ℕ k →
  is-simple-Undirected-Graph (standard-polygon-Undirected-Graph k)
pr1 (is-simple-standard-polygon-Undirected-Graph k H) p (pair x (pair y α)) =
  is-emb-is-injective
    {!!}
    {!!}
pr2 (is-simple-standard-polygon-Undirected-Graph k H) p = {!!}
-}
```
