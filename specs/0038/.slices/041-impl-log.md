# Step 041 — impl-log (ADD_CONTRACT STORE_XDUO_0008 SeedingProxy)

## Progress

- **Read** the system prompt (`add_contract_worker.system-md` + its base
  `arc-runner.system-md`, found under
  `AI-Strategy-Generator/src/ai_strategy_generator/multistep/`, NOT the repo-root
  path the task file names), the Berreman project prompt, and the slice spec. The
  `## Operator note` section is empty.
- **Surveyed** the seed values + their types: `MaterialLibrary.standardCategories`
  (`MaterialCategory list`) / `builtInEntries` (`MaterialEntry list`), and
  `Library.SeedSamples.all` (`Sample list`) / `Library.seedEntries`
  (`LibraryEntry list`) — all public module-level `let` values. The spec's line
  numbers had drifted (`SeedSamples` is at `ElementId.fs:511`, `all` at `:651`,
  `seedEntries` at `:665`; `standardCategories` at `MaterialLibrary.fs:118`,
  `builtInEntries` at `:368`) — same values, different lines. Confirmed no
  pre-existing `Seeding` / `SeedingProxy` symbol or project.
- **Studied** the ADD_CONTRACT precedent (`ExperimentDataProxy.fs` +
  `ExperimentDataProxyTests.fs`, `SceneProxyTests`) for the
  `[<ReferenceEquality>]`-proxy + inline-mock + mock-driven-test shape, and the
  `OpticalConstructor.Storage` fsproj for the class-library project template.
- **Created** the new `OpticalConstructor.Seeding` project (fsproj + `Seeding.fs`),
  registered it in `Berreman.slnx`, added the mock-driven `SeedingProxyTests.fs`,
  and wired the test project's compile item + project reference.
- **Verified locally** (advisory — the arc-runner's gate engine is the authority):
  the whole solution builds Release (0 errors; no warning from either new file), and
  the constructor test project is green at 669 passed / 0 failed (666 → 669, the
  three new facts).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Seeding/OpticalConstructor.Seeding.fsproj`
  — **new**. The class-library project (net10.0, x64, `--warnaserror+:25`,
  `AssemblyName = Softellect.Berreman.OpticalConstructor.Seeding`), one
  `ProjectReference` to `OpticalConstructor.Domain`. Mirrors the
  `OpticalConstructor.Storage` fsproj shape.
- `Berreman/OpticalConstructor/OpticalConstructor.Seeding/Seeding.fs`
  — **new**. `namespace OpticalConstructor.Seeding`, `module Seeding`: the
  `SeedingError = SeedRejected of reason : string` channel, the
  `[<ReferenceEquality>] SeedingProxy` (`saveCategory` / `saveMaterial` /
  `saveSample` / `saveLibraryEntry`, each `<X> -> Result<unit, SeedingError>`), and
  the pure `seedAll : SeedingProxy -> Result<unit, SeedingError>` pushing the
  EXISTING Domain seeds in category → material → sample → entry order. DECLARED
  lifecycle — the seam + orchestrator only, no real store.
- `Berreman/Berreman.slnx`
  — registered `OpticalConstructor.Seeding.fsproj` (x64 platform), after the Domain
  entry so the `build` gate compiles it.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SeedingProxyTests.fs`
  — **new**. The recording stub `SeedingProxy` + the mock-driven test proving
  `seedAll` pushes every seed value exactly once, in category → material → sample →
  entry order, through the exact signatures; plus the reference-equality fact and a
  `SeedRejected` short-circuit fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `SeedingProxyTests.fs` (after `TestDbMigrationTests.fs`) and a
  `ProjectReference` to `OpticalConstructor.Seeding`.

## Testing state

`commit_ready: true`. Per the ADD_CONTRACT **Invariant 6 — act only**, the worker
authors the contract, mock, and test; the arc-runner's deterministic gate engine is
the sole gate authority and re-runs every gate after this session exits. `CLAUDE.md`
mandates a green build and running tests after every change, so I ran build + the
constructor test project locally to verify MY OWN work only (advisory, de-risking a
failure-budget burn) — not to green-light a gate:

- **build** (whole solution, Release): **0 errors**. The 10 warnings are all
  pre-existing / third-party (`Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125 ×4,
  `ChartWindow.fs` FS0044, MathNet `SYSLIB0051` ×2, `Wolfram.NETLink` `NU1701` ×2 —
  the exempt `NU1701` plus warnings that predate this slice). **Neither new file
  (`Seeding.fs`, `SeedingProxyTests.fs`) emits any warning**, and the new
  `OpticalConstructor.Seeding` project compiled clean.
- **constructor-unit-tests**: green — **669 passed / 0 failed / 0 skipped** (666
  baseline + 3 new `SeedingProxyTests` facts; confirmed the 3 pass in isolation via
  `--filter FullyQualifiedName~SeedingProxyTests`).
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  new-project + Tests-only change (no touch to Berreman core, Ui, or Ui.Tests);
  the whole solution — Ui / Ui.Tests included — compiled clean in the build gate.
  Carried forward from the step-040 baseline. The arc-runner re-runs every gate.

Acceptance met: `SeedingProxyTests` builds a recording stub `SeedingProxy`, runs
`seedAll`, and proves each channel received EXACTLY its seed list — same count, the
SAME instances (`Object.ReferenceEquals`), in order — with a global call-order tape
pinning the category → material → sample → entry ordering across the four channels;
`seedAll` is bound to an explicitly-typed local so the compiler pins its signature,
and the stub is `: SeedingProxy`-annotated so each field is checked against the exact
declared signature. The test passes.

## Artifacts

None produced this round (a pure contract-declaration slice — no captured logs,
screenshots, or traces). The per-arc artifacts folder is
`C:\GitHub\Berreman\specs\0038\.artifacts`.

## Gotchas

- **DECLARED, not implemented.** No real store, no persistence, no `createInMemory`,
  no consumer wiring this round — the `SeedingProxy` seam + the pure `seedAll`
  orchestrator only. A later `IMPLEMENT_CONTRACT STORE_XDUO_0008` supplies the real
  store (e.g. the EFC-backed database from step 040) behind this surface, leaving
  `seedAll` unchanged.
- **Seeds STAY where they live.** `seedAll` only READS the existing Domain
  module-level values (`standardCategories`, `builtInEntries`, `SeedSamples.all`,
  `seedEntries`) and hands each through the proxy — it never re-defines a seed. The
  seeded Guids are frozen (future foreign keys); the identity that crosses the seam
  is exactly the identity the Domain already froze.
- **Reference identity, not `=`, in the test.** `MaterialEntry.properties`
  (`OpticalPropertiesWithDisp`) and the `LibraryEntry` payloads carry function-valued
  dispersion, so structural `=` on those seed types is unsafe. Because each seed
  collection is a module-level `let` (computed once), `Object.ReferenceEquals` between
  a recorded value and the seed list's own element holds — and it is the strongest
  proof that the SAME frozen seed flowed through (not a value-equal copy). The
  `box`-then-`ReferenceEquals` form keeps the generic helper compiling for any item
  type.
- **Order is fixed by foreign-key dependency.** category → material → sample → entry:
  an entry may reference a sample, a sample a material, a material a category. The
  `seedAll` chain and the test's expected call-order tape both encode this; the first
  `SeedRejected` short-circuits the chain (proved by the reject-first-material fact —
  categories all land, no sample/entry does).
- **New module path is `OpticalConstructor.Seeding.Seeding`.** namespace
  `OpticalConstructor.Seeding` + `module Seeding`; consumers `open
  OpticalConstructor.Seeding.Seeding` (mirrors the Domain's `namespace … + module …`
  precedent). No symbol collision — no prior `Seeding` name existed.
- **`--warnaserror+:25` / zero-warnings.** The inner `pushEach` is a genuinely
  generic `Result` combinator (permitted by CLAUDE.md), instantiated at four types,
  so it generalizes cleanly with no FS0064 / FS0025; interpolated strings only, no
  `sprintf`.
- **LF/CRLF.** The whole working tree is checked out CRLF-on-disk (every existing
  `.fs` / `.fsproj` / `.slnx` too); `.gitattributes` pins `eol=lf`, so git normalizes
  on commit. `git diff --numstat` equals `--ignore-cr-at-eol` for both edited tracked
  files (3/0 and 9/0 — pure additions), confirming NO CRLF-only churn; the new
  untracked files normalize to LF on add like every other file.
- **System-prompt path drift.** The task file lists the system prompt at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, but it actually
  lives at `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md`
  (with its `arc-runner.system-md` base alongside). No scope impact.
