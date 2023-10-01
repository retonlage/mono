import Lake
open Lake DSL

package «lean_autodiff» {
  -- add package configuration options here
}

lean_lib «LeanAutodiff» {
  -- add library configuration options here
}

@[default_target]
lean_exe «lean_autodiff» {
  root := `Main
}
