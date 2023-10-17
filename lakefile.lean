import Lake
open Lake DSL Lean


inductive SupportedArch
  | x86_64
  | arm64


def getArch? : BaseIO (Option SupportedArch) := do
  let proc := IO.Process.output {cmd := "uname", args := #["-m"], stdin := .null}
  let _ ← proc.toBaseIO
  return some .x86_64


elab "is_arm?" : term => do
  return toExpr <| (← getArch?).map (· matches .arm64)


package LeanInfer {
  
}

@[default_target]
lean_lib LeanInfer {
}
