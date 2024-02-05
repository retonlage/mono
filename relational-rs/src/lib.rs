use std::collections::HashMap;

struct Pairwise<SubIt>
where
    SubIt: Iterator,
    SubIt::Item: Clone,
{
    subit: SubIt,
    last: Option<SubIt::Item>
}

impl<'a, SubIt> Iterator for Pairwise<SubIt>
where
    SubIt: Iterator,
    SubIt::Item: Clone
{
    type Item = (SubIt::Item, SubIt::Item);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(last) = self.last.clone() {
            Some((last, self.subit.next()?))
        } else {
            let first = self.subit.next()?;
            let second = self.subit.next()?;
            self.last = Some(second.clone());
            Some((first, second))
        }
    }
}

impl<SubIt> Pairwise<SubIt>
where
    SubIt: Iterator,
    SubIt::Item: Copy,
{
    fn new(it: SubIt) -> Self {
        Pairwise {
            subit: it,
            last: None,
        }
    }
}

type VarID = u32;
type ClauseID = usize;
type VarCount = u32;

#[derive(Clone, PartialEq, Eq)]
enum Term<VarKind> {
    Var(VarKind),
    Const(u32),
    Pair(Box<Term<VarKind>>, Box<Term<VarKind>>),
}

type ClauseTerm = Term<VarID>;
type TermInstance = Term<(VarID, VarCount)>;

enum Clause {
    Conj(Vec<ClauseID>),
    Disj(Vec<ClauseID>),
    Unif(Vec<ClauseTerm>),
    Intr(Vec<VarID>, ClauseID),
}

#[derive(Clone)]
struct Head {
    clause: ClauseID,
    counters: HashMap<VarID, VarCount>,
}

impl Head {
    fn equip(&self, term: &ClauseTerm) -> TermInstance {
        match term {
            Term::Var(var_id) => {
                let count = if let Some(count) = self.counters.get(var_id) {
                    count.clone()
                } else {
                    0
                };
                Term::Var((*var_id, count))
            }
            Term::Const(const_id) => Term::Const(*const_id),
            Term::Pair(term1, term2) => {Term::Pair(Box::new(self.equip(term1)), Box::new(self.equip(term2)))
            },
        }
    }
}

#[derive(PartialEq)]
enum UnificationResult {
    Success, Failure
}

struct Subst {
    bindings: HashMap<(VarID, VarCount), TermInstance>,
    heads: Vec<Head>,
}

impl Subst {
    fn follow(&self, term: &TermInstance) -> TermInstance {
        if let TermInstance::Var(var_reference) = term {
            if let Some(query_result) = self.bindings.get(var_reference) {
                self.follow(query_result)
            } else {
                term.clone()
            }
        } else {
            term.clone()
        }
    }

    fn unify(&mut self, term1: &TermInstance, term2: &TermInstance) -> UnificationResult {
        if *term1 == *term2 {
            UnificationResult::Success
        } else if let TermInstance::Var((var_id, var_count)) = term1 {
            self.bindings.insert((*var_id, *var_count), term2.clone());
            UnificationResult::Success
        } else if let TermInstance::Var(_) = term2 {
            self.unify(term2, term1)
        } else if let (TermInstance::Pair(term11, term12), TermInstance::Pair(term21, term22)) = (term1, term2) {
            if self.unify(term11, term21) == UnificationResult::Success &&
                self.unify(term12, term22) == UnificationResult::Success {
                    UnificationResult::Success
                } else {
                    UnificationResult::Failure
                }

        } else {
            UnificationResult::Failure
        }
    }

    fn unify_many(&mut self, terms: &Vec<TermInstance>) -> UnificationResult {
        let pairwise = Pairwise::new(terms.iter());
        for (term1, term2) in pairwise {
            let unif_result = self.unify(&term1, &term2);
            if unif_result == UnificationResult::Failure {
                return UnificationResult::Failure;
            };
        }
        UnificationResult::Success
    }
}

struct Megastruct {
    clauses: Vec<Clause>,
    substs: Vec<Subst>,
}

impl Megastruct {
    fn eval(&mut self, select: fn(&Megastruct) -> (usize, usize)) {
        let (sel_subst_idx, sel_head_idx) = select(self);
        let clause: &Clause = self.clauses.get(self.substs[sel_subst_idx].heads[sel_head_idx].clause).unwrap();
        match clause {
            Clause::Conj(subclauses) => {
                if subclauses.len() > 0 {
                    head.clause = subclauses[0];
                    let subclauses_tail = &subclauses[1..];
                    let new_heads = subclauses.iter().map(|subclause| Head {
                        clause: *subclause,
                        counters: head.counters.clone(),
                    });
                    subst.heads.append(&mut new_heads.collect());
                } else {
                    subst.heads.swap_remove(sel_head_idx);
                }
            },
            Clause::Disj(subclauses) => {
                if subclauses.len() > 0 {
                    subst.heads[sel_head_idx].clause = subclauses[0];
                    let subclauses_tail = &subclauses[1..];
                    let new_substs = subclauses.iter().map(|subclause| Subst {
                        bindings: subst.bindings.clone(),
                        heads: subst.heads.clone()
                    });
                    self.substs.append(&mut new_substs.collect())
                } else {
                    subst.heads.swap_remove(sel_subst_idx);
                }
            },
            Clause::Intr(vars, subclause) => {
                for var in vars {
                    head.counters[var] = if let Some(count) = head.counters.get(var) {
                        count + 1
                    } else {
                        1
                    }
                };
                head.clause = *subclause;
            },
            Clause::Unif(vars) => {
                let equipped = vars.iter().map(|var| head.equip(var));
                let followed: Vec<TermInstance> = equipped.map(|var| subst.follow(&var)).collect();
                let unif_result = subst.unify_many(&followed);
                if unif_result == UnificationResult::Success {
                    subst.heads.swap_remove(sel_head_idx);
                } else {
                    self.substs.remove(sel_subst_idx);
                }
            },
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // fn it_works() {}
}
