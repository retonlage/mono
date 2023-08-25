# Reflexive graphs

```agda
module graph-theory.reflexive-graphs where
```

<details><summary>Imports</summary>

```agda
open import foundation.dependent-pair-types
open import foundation.universe-levels
```

</details>

## Idea

A reflexive graph is a graph such that there is an loop edge at every vertex.

## Definition

```agda
Reflexive-Graph : (l1 l2 : Level) → UU (lsuc l1 ⊔ lsuc l2)
Reflexive-Graph l1 l2 =
  Σ (UU l1) (λ V → Σ (V → V → UU l2) (λ E → (v : V) → E v v))

module _
  {l1 l2 : Level} (G : Reflexive-Graph l1 l2)
  where

  vertex-Reflexive-Graph : UU l1
  vertex-Reflexive-Graph = pr1 G

  edge-Reflexive-Graph : vertex-Reflexive-Graph → vertex-Reflexive-Graph → UU l2
  edge-Reflexive-Graph = pr1 (pr2 G)

  refl-Reflexive-Graph : (x : vertex-Reflexive-Graph) → edge-Reflexive-Graph x x
  refl-Reflexive-Graph = pr2 (pr2 G)
```
