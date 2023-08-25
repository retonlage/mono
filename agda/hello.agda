module hello where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Fin using (Fin; zero; suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)

infixr 5 _∷_

variable
  A : Set
  n : ℕ
  
lookup : Vec A n → Fin n → A
lookup (x ∷ _)  zero    = x
lookup (_ ∷ xs) (suc i) = lookup xs i

assoc-statement : Set
assoc-statement = ∀ (x y z : ℕ) -> (x + y) + z ≡ x + (y + z)

assoc-proof : ∀ (x y z : ℕ) -> (x + y) + z ≡ x + (y + z)
assoc-proof zero y z = refl
assoc-proof (suc x) y z = cong suc (assoc-proof x y z)
