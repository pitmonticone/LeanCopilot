import Lake
open Lake DSL
open System Lean Elab


inductive SupportedOS where
  | linux
  | macos
deriving Inhabited, BEq


def getOS : IO SupportedOS := do
  if Platform.isWindows then
    error "Windows is not supported"
  if Platform.isOSX then
    return .macos
  else
    return .linux


inductive SupportedArch where
  | x86_64
  | arm64
deriving Inhabited, BEq


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


structure SupportedPlatform where
  os : SupportedOS
  arch : SupportedArch


def getPlatform : IO SupportedPlatform := do
  if Platform.numBits != 64 then
    error "Only 64-bit platforms are supported"
  return ⟨← getOS, ← getArch⟩


package LeanInfer where
  preferReleaseBuild := get_config? noCloudRelease |>.isNone
  precompileModules := true
  buildType := BuildType.debug
  buildArchive? := is_arm? |>.map (if · then "arm64" else "x86_64")
  moreLinkArgs := #[s!"-L{__dir__}/build/lib", "-lstdc++"]


@[default_target]
lean_lib LeanInfer {
}


def afterReleaseSync (pkg : Package) (build : SchedulerM (Job α)) : IndexBuildM (Job α) := do
  if pkg.preferReleaseBuild ∧ pkg.name ≠ (← getRootPackage).name then
    (← pkg.release.fetch).bindAsync fun _ _ => build
  else
    build


def buildCpp (pkg : Package) (path : FilePath) (deps : List (BuildJob FilePath)) : SchedulerM (BuildJob FilePath) := do
  let optLevel := if pkg.buildType == .release then "-O3" else "-O0"
  let mut flags := #["-fPIC", "-std=c++11", "-stdlib=libc++", optLevel]
  match get_config? targetArch with
  | none => pure ()
  | some arch => flags := flags.push s!"--target={arch}"
  let args := flags ++ #["-I", (← getLeanIncludeDir).toString, "-I", (pkg.buildDir / "include").toString]
  let oFile := pkg.buildDir / (path.withExtension "o")
  let srcJob ← inputFile <| pkg.dir / path
  buildFileAfterDepList oFile (srcJob :: deps) (extraDepTrace := computeHash flags) fun deps =>
    compileO path.toString oFile deps[0]! args "clang++"


target generator.o pkg : FilePath := do
  let build := buildCpp pkg "generator.cpp" []
  afterReleaseSync pkg build


extern_lib libleanffi pkg := do
  let name := nameToStaticLib "leanffi"
  let oGen ← generator.o.fetch
  buildStaticLib (pkg.nativeLibDir / name) #[oGen]
