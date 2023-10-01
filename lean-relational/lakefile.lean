import Lake
open Lake DSL

require std from git
  "https://github.com/leanprover/std4/" @ "28459f7"

package «relational» {
  -- add package configuration options here
}

@[default_target]
lean_lib «relational» {
  roots := #[`relational]
}
