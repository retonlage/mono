def All (l : List Prop) : Prop :=
match l with
| [] => True
| [x :: xs] => x ∧ proof_all xs

def pair_window : (List α) → List (α × α)
| [] => []
| [_] => []
| x :: y :: xs => (x, y) :: pair_window (y :: xs)

inductive ComparisonResult where
  | lt | eq | gt

class Comparable (α : Type) where
  compare : α → α → ComparisonResult
  transitivity : ∀ a b c : α, compare a b = lt → compare b c = lt → compare a c = lt

def is_sorted [Comparable α] (list : List α) : Prop :=
        list = [] ∨
        list = [_] ∨
        ((pair_window list).map (λ (x, y) => compare x y = ComparisonResult.lt))

-- theorem stable_sort_is_unique : ∀ α : Comparable,
