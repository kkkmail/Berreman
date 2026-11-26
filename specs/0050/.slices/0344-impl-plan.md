# Step 0344 — impl plan

Move the repository-root kit locator `CommittedKit` out of `BerremanTests` into a new shared test-support
library, `Berreman/BerremanTestSupport/` (spec 0050 S10.2, obligation S102-KIT-LOCATOR-MOVE).

1. Create `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj`: a plain net10.0 x64 library with the two
   property groups of `OpticalConstructor.Optimization.fsproj` minus `AssemblyName` (default assembly name
   `BerremanTestSupport`), compiling `CommittedKits.fs`, referencing `OpticalConstructor.Domain.fsproj`; no
   package reference, no comment, LF.
2. `Berreman/Berreman.slnx`: add the project entry right after `BerremanTests/BerremanTests.fsproj`.
3. Move `Berreman/BerremanTests/CommittedKits.fs` to `Berreman/BerremanTestSupport/CommittedKits.fs` (a plain
   file move; the worker protocol forbids git-mutation commands, so `git mv` is not run — the commit records the
   rename), namespace `BerremanTestSupport`, doc rewritten per the brief, body unchanged.
4. `BerremanTests.fsproj`: drop `<Compile Include="CommittedKits.fs" />`, add the project reference to
   `..\BerremanTestSupport\BerremanTestSupport.fsproj` after the last `ProjectReference`.
5. `MuellerReconstructionTests.fs`: add `open BerremanTestSupport` after its last `open`.

Risks: the new assembly is outside `inScopePatterns`, so elevation reports nothing for it; `FolderName` reaches
the library transitively through Domain's `Softellect.Sys` package reference. Verify build, elevation and the
selected `MuellerReconstructionTests` class locally.
