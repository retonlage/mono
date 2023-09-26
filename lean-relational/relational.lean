import Std.Data.RBMap

def VarID := Nat
  deriving BEq

def Symbol := Nat
  deriving BEq

def Generation := Nat
  deriving BEq

def Var := VarID × Generation
  deriving BEq

def ClauseID := Nat
  deriving BEq

inductive Clause where
  | Conjunction  : List ClauseID → Clause
  | Disjunction  : List ClauseID → Clause
  | Introduction : ClauseTerm → Clause
  | Unification  : ClauseTerm → ClauseTerm → Clause

inductive ClauseTerm where
  | Var (id : VarID)
  | Symbol (id : Symbol)
  | Pair (subterms: ClauseTerm × ClauseTerm)

inductive TermValue where
  | Var (term : Var)
  | Symbol (id : Symbol)
  | Pair (subterm₁ subterm₂ : TermValue)

structure Substitution where
  bindings : RBMap Var TermValue
  counters : List (VarID × Generation)

namespace Substitution

def get_in_counters

def var_equip_generation (subst : Substitution) (var : VarID) : Var :=
| subst =>  subst.bindings


def get_in_bindings : List (Var × TermValue) → Var → Option TermValue
| (key, val) :: bindings_tail, lookup
  => if key == lookup
     then some val
     else get_in_bindings bindings_tail lookup
| List.nil, _ => none

def get : Substitution → Var → Option TermValue
| subst => get_in_bindings subst.bindings

def follow (subst : Substitution) (var_id : VarID) : TermValue :=
let var :=
match subst.get var with
| some result => sorry
| none => subst.counters.

-- assumes that terms are both dereferenced already
def unify (subst : Substitution) : TermValue → TermValue → Substitution
|
