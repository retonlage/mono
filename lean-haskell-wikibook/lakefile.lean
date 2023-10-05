import Lake
open Lake DSL

package «lean-haskell-wikibook» {
  -- add package configuration options here
}

lean_lib «LeanHaskellWikibook» {
  -- add library configuration options here
}

@[default_target]
lean_exe «lean-haskell-wikibook» {
  root := `Main
}
