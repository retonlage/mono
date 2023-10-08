import Std.Data.RBMap
import Mathlib.Data.List.Basic

def List.body : ∀ (xs : List α), xs ≠ [] → List α
| [], h => absurd rfl h
| [_], _ => []
| (x :: b :: xs), _ =>
        List.cons x (List.body (b::xs) (λ h => List.noConfusion h))

def Std.RBMap.change! [Inhabited β] (rb_map : Std.RBMap α β cmp) (key : α) (f : β → β) : Std.RBMap α β cmp :=
def Std.RBMap.change! [Inhabited β] (rb_map : Std.RBMap α β comp) (key : α) (f : β → β) : Std.RBMap α β comp :=
let old_val := rb_map.find! key;
let new_val := f old_val;
rb_map.insert key new_val

def VarID := Nat
  deriving BEq, Ord, Inhabited, Repr

def Symbol := Nat
  deriving BEq, Ord, Inhabited, Repr

def Generation := Nat
  deriving BEq, Ord, Inhabited, Repr

instance : HAdd Generation Generation Generation where
  hAdd := Nat.add

def Var := VarID × Generation
  deriving BEq, Repr

instance : OfNat Generation n where
  ofNat := n

instance : OfNat Symbol n where
  ofNat := n

instance : OfNat VarID n where
  ofNat := n

instance : Ord Var where
  compare : (Var) → (Var) → Ordering
  | (id₁, gen₁), (id₂, gen₂) =>
    match compare id₁ id₂ with
     | Ordering.eq => compare gen₁ gen₂
     | Ordering.lt => Ordering.lt
     | Ordering.gt => Ordering.gt

def ClauseID := Nat
  deriving BEq, Ord, Repr

instance : OfNat ClauseID n where
  ofNat := n

inductive ClauseTerm where
  | Var (var : VarID)
  | Symbol (id : Symbol)
  | Pair (subterm₁ subterm₂ : ClauseTerm)

inductive Clause where
  | Conjunction  : List ClauseID → Clause
  | Disjunction  : List ClauseID → Clause
  | Introduction : VarID → ClauseID → Clause
  | Unification  : ClauseTerm → ClauseTerm → Clause
  deriving Inhabited

inductive TermValue where
  | Var (term : Var)
  | Symbol (id : Symbol)
  | Pair (subterm₁ subterm₂ : TermValue)
  deriving BEq, Repr

def Bindings := Std.RBMap Var TermValue Ord.compare
  deriving Repr
namespace Bindings

def get : Bindings → Var → Option TermValue
| bindings => bindings.find?

partial def follow (bindings : Bindings) (term : TermValue) : TermValue :=
if let TermValue.Var var := term then
  if let some next := bindings.get var then
    bindings.follow next
  else
    term
else term

-- assumes that terms are both dereferenced already
partial def unify (bindings : Bindings) (term1 term2 : TermValue) : Option Bindings :=
if term1 == term2 then
  some bindings
else if let TermValue.Var var1 := term1 then
  some (bindings.insert var1 term2)
else if let TermValue.Var var2 := term2 then
  unify bindings (TermValue.Var var2) term1
else if let TermValue.Pair lhs₁ lhs₂ := term1 then
          if let TermValue.Pair rhs₁ rhs₂ := term2 then
            let head_bound := unify bindings lhs₁ rhs₁;
            if let some head_bound := head_bound then
              unify head_bound lhs₂ rhs₂
            else none -- heads do not unify
          else none -- only lhs is a pair
else
  none -- terms can't be bound

end Bindings

structure Head where
  counters : Std.RBMap VarID Generation Ord.compare
  clause : ClauseID
  deriving Repr

namespace Head

def var_equip_generation (head : Head) (var : VarID) : Var :=
  let counter := head.counters.findD var 0;
  (var, counter)

def incr_counter (head : Head) (var : VarID) : Head :=
  { head with counters := head.counters.change! var (λ gen => gen + 1) }

def to_term_value (head : Head) : ClauseTerm → TermValue
| ClauseTerm.Var var_id => TermValue.Var (head.var_equip_generation var_id)
| ClauseTerm.Symbol id => TermValue.Symbol id
| ClauseTerm.Pair subterm₁ subterm₂ => TermValue.Pair (head.to_term_value subterm₁) (head.to_term_value subterm₂)

end Head

def LogicGraph := Std.RBMap ClauseID Clause Ord.compare
namespace LogicGraph

structure Substitution where
  bindings : Bindings
  heads : List Head

  heads_nonempty : heads ≠ []

open Clause

def eval (graph : LogicGraph)
         (bindings : Bindings)
         (head : Head)
         (other_heads : List Head)
    : (List (Bindings × List Head) × Option Bindings) :=
let clause := graph.find! head.clause; -- TODO: use find with proofs instead
match clause with
| Conjunction subclauses => ([(bindings, other_heads.append (List.map (λ subclause => { head with clause := subclause}) subclauses))], none)
| Disjunction subclauses => (List.map (λ subclause => (bindings, other_heads.concat { head with clause := subclause})) subclauses, none)
| Introduction var_id subclause => ([(bindings, other_heads.concat {(head.incr_counter var_id) with clause := subclause})], none)
| Unification term1 term2 => match Bindings.unify bindings
                                                  (bindings.follow (head.to_term_value term1))
                                                  (bindings.follow (head.to_term_value term2)) with
  | some new_bindings => if other_heads.length == 0 then
                         ([], some new_bindings)
                         else ([(new_bindings, other_heads)], none)
  | none => ([], none)

def Select := LogicGraph → (substs : List Substitution) → substs ≠ [] → (Bindings × Head × List Head)

def select_depth_first : Select :=
  λ _ substs substs_nonempty => let selected_subst := substs.head substs_nonempty;
                                (selected_subst.bindings,
                                 selected_subst.heads.head selected_subst.heads_nonempty,
                                 selected_subst.heads.tailD [])

def select_breadth_first : Select :=
  λ _ substs substs_nonempty => let selected_subst := substs.getLast substs_nonempty;
                                (selected_subst.bindings,
                                 selected_subst.heads.getLast selected_subst.heads_nonempty,
                                 selected_subst.heads.body selected_subst.heads_nonempty)


partial def eval_to_next (graph : LogicGraph)
                         (substs : List (Bindings × List Head))
                         (substs_nonempty : substs ≠ [])
                         (selectF : Select)
    : Option (Bindings × List (Bindings × List Head)) :=
let (bindings, heads) := selectF graph substs substs_nonempty;
let (new_substitutions, results_option) := eval graph bindings heads;
if let some(results) := results_option; then
  some ()
else
  EvalResult.More new_substitutions bindings

partial def eval_all (graph : LogicGraph)
                     (selectF : Select)
                     (entry_point : ClauseID)
                     (entry_point_is_in_graph : entry_point ∈ graph)
                     : EvalResult :=
 let entry_head : Head := { counters := Std.RBMap.empty, clause := entry_point };
 let entry_substs : List (Bindings × List Head) := [(Std.RBMap.empty, [])];


end LogicGraph



def Std.RBMap.fromList [BEq α] [Ord α] (l : List (α × β)) : Std.RBMap α β Ord.compare :=
  l.foldl (λ map (key, value) => map.insert key value) Std.RBMap.empty

def test_logic_graph_unif : LogicGraph :=
    Std.RBMap.fromList [(0, Clause.Unification
                              (ClauseTerm.Var 1)
                              (ClauseTerm.Symbol 1))]

def test_logic_graph_unif_under_intro : LogicGraph :=
    Std.RBMap.fromList [(0, Clause.Introduction 1 1),
                        (1, Clause.Unification
                              (ClauseTerm.Var 1)
                              (ClauseTerm.Symbol 1))]

def test_bindings : Bindings := Std.RBMap.empty

def test_head : Head := { counters := Std.RBMap.empty, clause := 0 }

#eval LogicGraph.eval test_logic_graph_unif_under_intro test_bindings test_head []
