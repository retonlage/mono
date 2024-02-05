import Std.Data.RBMap
import Mathlib.Data.List.Basic
import Mathlib.Tactic.Ring
import Mathlib.Tactic.Linarith

def List.body : (xs : List α) → xs ≠ [] → List α
| [], h => absurd rfl h
| [_], _ => []
| (x :: b :: xs), _ =>
        List.cons x (List.body (b::xs) (λ h => List.noConfusion h))

theorem Nat.succ_lt_succ_if {n m : Nat} : Nat.succ n < Nat.succ m → n < m := by
  exact Nat.succ_lt_succ_iff.mp

def List.extractIdx : (xs : List α) → (idx : Fin (xs.length)) → α × List α
| (x :: xs), ⟨0, _⟩ => (x, xs)
| (x :: xs), ⟨n+1, h⟩ => let ⟨y, ys⟩ := extractIdx xs ⟨n, Nat.succ_lt_succ_if h⟩;
                         (y, x :: ys)

lemma List.extractIdx_eq_get : (xs : List α) → (idx : Fin (xs.length)) → Prod.fst (xs.extractIdx idx) = xs.get idx := by
  intro xs idx
  induction xs with
  | nil => cases idx.2
  | cons x xs ih =>
    cases idx.val with
    | zero => sorry
    | succ n => sorry --simp [List.extractIdx, ih ⟨n, extractIdx_lengths_correct h⟩]

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
  compare : Var → Var → Ordering
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

def empty : Bindings := Std.RBMap.empty

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

instance : Membership ClauseID LogicGraph where
  mem id graph := graph.contains id

namespace LogicGraph

structure Substitution where
  bindings : Bindings
  heads : List Head

  heads_nonempty : heads ≠ []
  deriving Repr

open Clause

def eval (graph : LogicGraph)
         (bindings : Bindings)
         (head : Head)
         (other_heads : List Head)
    : (List Substitution × Option Bindings) :=
let clause := graph.find! head.clause; -- TODO: use find with proofs instead
match clause with
| Conjunction subclauses => ([{
                         bindings := bindings,
                         heads := other_heads.append (List.map (λ subclause => { head with clause := subclause}) subclauses),
                         heads_nonempty := sorry
                         }], none)
| Disjunction subclauses => (List.map (λ subclause => {
                          bindings := bindings,
                          heads := other_heads.concat { head with clause := subclause},
                          heads_nonempty := sorry})
                          subclauses, none)
| Introduction var_id subclause => ([{
                          bindings := bindings,
                          heads := other_heads.concat {(head.incr_counter var_id) with clause := subclause}
                          heads_nonempty := sorry }
                          ], none)
| Unification term1 term2 => match Bindings.unify bindings
                                                  (bindings.follow (head.to_term_value term1))
                                                  (bindings.follow (head.to_term_value term2)) with
  | some new_bindings => if other_heads.length == 0 then
                         ([], some new_bindings)
                         else ([{bindings := new_bindings, heads := other_heads, heads_nonempty := sorry}], none)
  | none => ([], none)

def Select := LogicGraph → (substs : List Substitution) → substs ≠ []
              → Σ selected_subst : Fin substs.length,
                  Fin (substs.get selected_subst).heads.length

lemma length_pos_of_nonempty {α : Type _} (list : List α) (h : list ≠ []) : 0 < list.length := by
  cases list with
  | nil  => contradiction
  | cons => exact Nat.zero_lt_succ _

def select_depth_first : Select :=
  λ _ substs substs_nonempty => let len_substs_gt_z := length_pos_of_nonempty substs substs_nonempty
                                ⟨⟨0, len_substs_gt_z⟩,
                                 ⟨0, let first_subst := substs.get ⟨0, len_substs_gt_z⟩
                                     length_pos_of_nonempty first_subst.heads first_subst.heads_nonempty⟩⟩

lemma last_index_less_than_length {α : Type _} (list : List α) (h : list ≠ []) :
            List.length list - 1 < List.length list := by
  cases list with
  | nil => contradiction
  | cons head tail => let list_len_ne_z : (List.cons head tail).length ≠ 0 := by
                        intro h
                        injection h
                      exact Nat.pred_lt list_len_ne_z

def select_breadth_first : Select :=
  λ _ substs substs_nonempty => let last_subst_idx := substs.length - 1
                                let last_index_lt_length := last_index_less_than_length substs substs_nonempty
                                let last_subst := substs.get ⟨last_subst_idx, last_index_lt_length⟩
                                let last_head_idx := last_subst.heads.length - 1
                                let last_head_index_lt_length := last_index_less_than_length last_subst.heads last_subst.heads_nonempty
                                ⟨⟨last_subst_idx, last_index_lt_length⟩,
                                 ⟨last_head_idx, last_head_index_lt_length⟩⟩

partial def eval_to_next (graph : LogicGraph)
                         (substs : List Substitution)
                         (substs_nonempty : substs ≠ [])
                         (selectF : Select)
    : Option (Bindings × List Substitution) :=
let ⟨sel_subst_idx, sel_head_idx⟩ := selectF graph substs substs_nonempty
let (sel_subst, remaining_substs) := substs.extractIdx sel_subst_idx
let sel_head_idx' : Fin (List.length sel_subst.heads) := ⟨sel_head_idx.val, sorry⟩; -- TODO: prove this
let (sel_head, remaining_heads) := sel_subst.heads.extractIdx sel_head_idx';
let (new_substitutions, results_option) := eval graph sel_subst.bindings sel_head remaining_heads;
if let some results := results_option then
  some (results, new_substitutions ++ remaining_substs)
else if remaining_substs.length == 0 then
  none
else eval_to_next graph (new_substitutions ++ remaining_substs) sorry selectF

partial def eval_all(graph : LogicGraph)
                         (substs : List Substitution)
                         (substs_nonempty : substs ≠ [])
                         (selectF : Select)
    : List Bindings :=
    if let some (result, next_substs) := eval_to_next graph substs sorry selectF then
      result :: eval_all graph next_substs sorry selectF
    else
      List.nil

def eval_from_entry (graph : LogicGraph)
                     (selectF : Select)
                     (entry_point : ClauseID)
                     -- (entry_point_is_in_graph : entry_point ∈ graph)
                     : List Bindings :=
 let entry_head : Head := { counters := Std.RBMap.empty, clause := entry_point };
 let entry_substs : List Substitution := [{bindings := Bindings.empty, heads := [entry_head], heads_nonempty := by simp}];
 eval_all graph entry_substs sorry selectF

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

#eval LogicGraph.eval_from_entry test_logic_graph_unif LogicGraph.select_depth_first 0
