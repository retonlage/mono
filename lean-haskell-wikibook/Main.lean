import «LeanHaskellWikibook»

class Monoid (m : Type) where
  id : m
  binop : m → m → m

  -- laws
  assoc : (x y z : m) → binop (binop x y) z = binop x (binop y z)
  l_id : (x : m) → binop id x = x
  r_id : (x : m) → binop x id = x

infixl:100 "<>" => Monoid.binop

class _Functor (F : Type u₁ → Type u₂) where
  fmap : {α β : Type u₁}  → (α → β) → F α → F β

  -- laws
  fmap_id : {a : F α} → fmap id a = id a
  fmap_comp : fmap (g ∘ f) = fmap g ∘ fmap f

class _Applicative (F : Type u₁ → Type u₂) where
  pure : α → F α
  apply : F (α → β) → F α → F β

  -- laws
  apply_id : {a : F α} → apply (pure id) a = a
  homeomorphism : {f : α → β} → apply (pure f) (pure a) = pure (f a)
