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

private def nameToVersionedSharedLib (name : String) (v : String) : String :=
  if Platform.isWindows then s!"{name}.dll"
  else if Platform.isOSX  then s!"lib{name}.{v}.dylib"
  else s!"lib{name}.so.{v}"


def getClangSearchPaths : IO (Array FilePath) := do
  let output ← IO.Process.output {
    cmd := "clang++", args := #["-v", "-lstdc++"]
  }
  let mut paths := #[]
  for s in output.stderr.splitOn do
    if s.startsWith "-L/" then
      paths := paths.push (s.drop 2 : FilePath).normalize
  return paths


def getLibPath (name : String) : IO (Option FilePath) := do
  let searchPaths ← getClangSearchPaths
  for path in searchPaths do
    let libPath := path / name
    if ← libPath.pathExists then
      return libPath
  return none


def afterReleaseSync (pkg : Package) (build : SchedulerM (Job α)) : IndexBuildM (Job α) := do
  if pkg.preferReleaseBuild ∧ pkg.name ≠ (← getRootPackage).name then
    (← pkg.release.fetch).bindAsync fun _ _ => build
  else
    build


def afterReleaseAsync (pkg : Package) (build : BuildM α) : IndexBuildM (Job α) := do
  if pkg.preferReleaseBuild ∧ pkg.name ≠ (← getRootPackage).name then
    (← pkg.release.fetch).bindSync fun _ _ => build
  else
    Job.async build


def copyLibJob (pkg : Package) (libName : String) : IndexBuildM (BuildJob FilePath) :=
  afterReleaseAsync pkg do
  if !Platform.isOSX then  -- Only required for Linux
    let dst := pkg.nativeLibDir / libName
    try
      let depTrace := Hash.ofString libName
      let trace ← buildFileUnlessUpToDate dst depTrace do
        let some src ← getLibPath libName | error s!"{libName} not found"
        logStep s!"Copying from {src} to {dst}"
        proc {
          cmd := "cp"
          args := #[src.toString, dst.toString]
        }
        -- TODO: Use relative symbolic links instead.
        proc {
          cmd := "cp"
          args := #[src.toString, dst.toString.dropRight 2]
        }
        proc {
          cmd := "cp"
          args := #[dst.toString, dst.toString.dropRight 4]
        }
      pure (dst, trace)
    else
      pure (dst, ← computeTrace dst)
  else
    pure ("", .nil)


target libcpp pkg : FilePath := do
  copyLibJob pkg "libc++.so.1.0"


target libcppabi pkg : FilePath := do
  copyLibJob pkg "libc++abi.so.1.0"


target libunwind pkg : FilePath := do
  copyLibJob pkg "libunwind.so.1.0"


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
  let cpp ← libcpp.fetch
  let cppabi ← libcppabi.fetch
  let unwind ← libunwind.fetch
  let build := buildCpp pkg "generator.cpp" [cpp, cppabi, unwind]
  afterReleaseSync pkg build

extern_lib libleanffi pkg := do
  let name := nameToStaticLib "leanffi"
  let oGen ← generator.o.fetch
  buildStaticLib (pkg.nativeLibDir / name) #[oGen]
