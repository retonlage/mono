structure AlgoM where
  op_count : Nat

instance : Monad AlgoM where
    pure a := λ _ => a
    x >>= g := λ
