import Lake
open Lake DSL

require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "81fa8d9"

package «relational» {
  -- add package configuration options here
}

@[default_target]
lean_lib «relational» {
  roots := #[`relational]
}
