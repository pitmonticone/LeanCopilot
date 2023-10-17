import Lean

open Lean Elab Tactic System

namespace LeanInfer

namespace Cache

def modelName := "onnx-leandojo-lean4-tacgen-byt5-small"
def modelURL := s!"https://huggingface.co/kaiyuy/{modelName}"

private def getHomeDir : IO FilePath := do
  let some dir ← IO.getEnv "HOME" | throw $ IO.userError "Cannot find the $HOME environment variable."
  return dir

def getDefaultCacheDir : IO FilePath := do
  return (← getHomeDir) / ".cache" / "lean_infer"

def getCacheDir : IO FilePath := do
  let defaultCacheDir ← getDefaultCacheDir
  let dir := match ← IO.getEnv "LEAN_INFER_CACHE_DIR" with
  | some dir => (dir : FilePath)
  | none => defaultCacheDir
  if !(← dir.pathExists)  then
    IO.FS.createDirAll dir
  return dir.normalize

def getModelDir : IO FilePath := do
  let cacheDir ← getCacheDir
  return cacheDir / modelName

private def checkAvailable (cmd : String) : IO Unit := do
  let proc ← IO.Process.output {
    cmd := "which",
    args := #[cmd]
  }
  if proc.exitCode != 0 then
    throw $ IO.userError s!"Cannot find `{cmd}`."

private def initGitLFS : IO Unit := do
  checkAvailable "git"
  let proc ← IO.Process.output {
    cmd := "git"
    args := #["lfs", "install"]
  }
  if proc.exitCode != 0 then
    throw $ IO.userError s!"Failed to initialize Git LFS. Please install it from https://git-lfs.com."

private def downloadModel : IO Unit := do
  let cacheDir ← getCacheDir
  initGitLFS
  let proc ← IO.Process.output {
    cmd := "git"
    args := #["clone", modelURL]
    cwd := cacheDir
  }
  if proc.exitCode != 0 then
    throw $ IO.userError s!"Failed to download the model. You download it manually from {modelURL} and store it in `{cacheDir}/`. See https://huggingface.co/docs/hub/models-downloading for details."

private def hasLocalChange (repoRoot : FilePath) : IO Bool := do
  checkAvailable "git"
  let proc ← IO.Process.output {
    cmd := "git"
    args := #["diff", "--shortstat"]
    cwd := repoRoot
  }
  return proc.exitCode == 0 ∧ proc.stdout != ""

def checkModel : IO Unit := do
  let modelDir ← getModelDir
  if ← hasLocalChange modelDir then
    IO.FS.removeDirAll modelDir
  if ¬(← modelDir.pathExists) then
    downloadModel

end Cache


namespace Core

@[extern "init_generator"]
private opaque init_generator (modelDir : @& String) : Bool 

@[extern "is_initialized"]
private opaque is_initialized : Unit → Bool

-- https://huggingface.co/docs/transformers/v4.28.1/en/main_classes/text_generation
@[extern "generate"]
private opaque generate (input : @& String) (numReturnSequences : UInt64) (maxLength : UInt64) 
(temperature : Float) (numBeams : UInt64) : Array (String × Float)

@[extern "encode"]
private opaque encode (input : @& String) : FloatArray

end Core

private def is_initialized : IO Bool := do
  return Core.is_initialized ()

private def init_generator : CoreM Bool := do
  if ← is_initialized then
    return true
  else if Core.init_generator (← Cache.getModelDir).toString then
    return true
  else
    logWarning  "Cannot find the generator model. If you would like to download it, run `suggest_tactics!` and wait for a few mintues."
    return false

def generate (input : String) (numReturnSequences : UInt64 := 8) 
(maxLength : UInt64 := 256) (temperature : Float := 1.0) 
(numBeams : UInt64 := 1) : CoreM (Array (String × Float)) := do
  if ← init_generator then
    return Core.generate input numReturnSequences maxLength temperature numBeams
  else
    return #[]

def encode (input : String) : IO FloatArray := do
  return Core.encode input

def retrieve (input : String) : IO (Array (String × Float)) := do
  let query ← encode input
  println! query
  return #[("hello", 0.5)]  -- Not implemented yet.

def ppTacticState : List MVarId → MetaM String
  | [] => return "no goals"
  | [g] => return (← Meta.ppGoal g).pretty
  | goals => 
      return (← goals.foldlM (init := "") (fun a b => do return s!"{a}\n\n{(← Meta.ppGoal b).pretty}")).trim

def getPpTacticState : TacticM String := do
  let goals ← getUnsolvedGoals
  ppTacticState goals

def suggestTactics : TacticM (Array (String × Float)) := do
  let input ← getPpTacticState
  let suggestions ← generate input
  return suggestions


end LeanInfer
