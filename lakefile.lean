import Lake
open Lake DSL System Lean Elab


inductive SupportedArch where
  | x86_64
  | arm64


def getArch? : BaseIO (Option SupportedArch) := do
  let proc := IO.Process.output {cmd := "uname", args := #["-m"], stdin := .null}
  let .ok output ← proc.toBaseIO
    | return none
  let arch := output.stdout.trim
  if arch ∈ ["arm64", "aarch64"] then
    return some .arm64
  else if arch == "x86_64" then
    return some .x86_64
  else
    return none


def getArch : IO SupportedArch := do
  if let some arch ← getArch? then 
    return arch 
  else
    error "Unknown architecture"


elab "is_arm?" : term => do
  return toExpr <| (← getArch?).map (· matches .arm64)


package LeanInfer {
  
}

@[default_target]
lean_lib LeanInfer {
}
