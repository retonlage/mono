# Orientations of undirected graphs

```agda
module graph-theory.orientations-undirected-graphs where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.universe-levels

open import graph-theory.undirected-graphs

open import univalent-combinatorics.finite-types
```

</details>

## Idea

An orientation of an undirected graph is a function that picks a direction for
every edge.

## Definition

```agda
module _
  {l1 l2 : Level} (G : Undirected-Graph l1 l2)
  where

  orientation-Undirected-Graph : UU (lsuc lzero ⊔ l1 ⊔ l2)
  orientation-Undirected-Graph =
    ( p : unordered-pair-vertices-Undirected-Graph G) →
    edge-Undirected-Graph G p → type-UU-Fin 2 (pr1 p)

  source-edge-orientation-Undirected-Graph :
    orientation-Undirected-Graph →
    (p : unordered-pair-vertices-Undirected-Graph G) →
    edge-Undirected-Graph G p → vertex-Undirected-Graph G
  source-edge-orientation-Undirected-Graph d (pair X p) e =
    p (d (pair X p) e)
```
