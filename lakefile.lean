import Lake
open Lake DSL

require Parser from git "https://github.com/fgdorais/lean4-parser" @ "main"

package «random_stock» where
  -- add package configuration options here

lean_lib «RandomStock» where
  -- add library configuration options here

@[default_target]
lean_exe «random_stock» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
