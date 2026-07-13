# Step 041 — impl-plan (ADD_CONTRACT STORE_XDUO_0008 SeedingProxy)

## Goal

Declare the seed-push seam `SeedingProxy` in a NEW F# class-library project
`OpticalConstructor.Seeding` (net10.0, x64; referenced from Domain), plus the pure
`seedAll` orchestrator that pushes the EXISTING Domain seed values through it, and a
mock-driven test in `OpticalConstructor.Tests`.

This is an **ADD_CONTRACT** step (DECLARED lifecycle): declare the surface + a
mock + a test that pins it. No real store, no persistence, no wiring.

## Approach

1. **New project** `Berreman/OpticalConstructor/OpticalConstructor.Seeding/`:
   - `OpticalConstructor.Seeding.fsproj` — net10.0, x64, `--warnaserror+:25`,
     `AssemblyName = Softellect.Berreman.OpticalConstructor.Seeding`, one
     `ProjectReference` to `OpticalConstructor.Domain`. Mirrors the
     `OpticalConstructor.Storage` fsproj shape exactly (same PropertyGroups / NoWarn).
   - `Seeding.fs` — `namespace OpticalConstructor.Seeding`, `module Seeding`:
     - `type SeedingError = SeedRejected of reason : string`
     - `[<ReferenceEquality>] type SeedingProxy = { saveCategory / saveMaterial /
       saveSample / saveLibraryEntry }`, each `<X> -> Result<unit, SeedingError>`
       over the elevated Domain types (`MaterialCategory` / `MaterialEntry` /
       `Sample` / `LibraryEntry`). `[<ReferenceEquality>]` because the fields are
       functions (the `LibraryProxy` / `ExperimentDataProxy` convention).
     - `let seedAll (proxy : SeedingProxy) : Result<unit, SeedingError>` — push the
       EXISTING seeds in category → material → sample → entry order:
       `MaterialLibrary.standardCategories`, `MaterialLibrary.builtInEntries`,
       `Library.SeedSamples.all`, `Library.seedEntries`. A private generic
       `pushEach` folds each list, short-circuiting on the first `Error` via
       `Result.bind`; the four groups chain with `Result.bind`. The seeds STAY where
       they live next to their types — `seedAll` only references them.

2. **Register the project** in `Berreman/Berreman.slnx` (x64 platform), so the
   `build` gate (`dotnet build Berreman.slnx`) compiles it.

3. **Test** `OpticalConstructor.Tests/SeedingProxyTests.fs`:
   - A recording stub `SeedingProxy` capturing each received value per channel plus a
     global call-order tape.
   - The acceptance fact: `seedAll recordingProxy` is `Ok ()`, and each channel
     received EXACTLY the corresponding seed list — same count, same instances (by
     `Object.ReferenceEquals`, since `MaterialEntry`/`LibraryEntry` carry
     no-structural-equality engine payloads), in order; and the global tape is
     category* → material* → sample* → entry* (proving the cross-channel order).
   - A reference-equality fact (mirrors the `[<ReferenceEquality>]` precedent).
   - A short-circuit fact: a proxy that rejects the first material returns that
     `SeedRejected` and pushes no samples/entries (exercises the `SeedingError`
     channel).
   - Register the test file + a `ProjectReference` to `OpticalConstructor.Seeding`
     in the Tests fsproj.

## Files to create / modify

- **new** `OpticalConstructor.Seeding/OpticalConstructor.Seeding.fsproj`
- **new** `OpticalConstructor.Seeding/Seeding.fs`
- **edit** `Berreman/Berreman.slnx` (add the project)
- **new** `OpticalConstructor.Tests/SeedingProxyTests.fs`
- **edit** `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (compile item + project ref)

## Risks / notes

- **Reference identity, not `=`.** `MaterialEntry.properties : OpticalPropertiesWithDisp`
  and the `LibraryEntry` payloads carry function-valued dispersion; structural `=` is
  unsafe. The seeds are module-level `let` values (computed once) so
  `Object.ReferenceEquals` between a recorded value and the seed list element holds —
  and it is the strongest proof the SAME seed flowed through.
- **DECLARED only** — no `createInMemory`, no persistence, no consumer wiring.
- **Compile / slnx order** — Seeding references Domain only; Tests references Seeding.
- **`--warnaserror+:25`** and zero-warnings-from-our-code: keep matches complete and
  no `sprintf`.
