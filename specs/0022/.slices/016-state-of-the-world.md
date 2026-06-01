# 016 state-of-the-world — UX shell III (Part J §J.10–J.12), LAST slice

## Where we are

Slice 016 is the FINAL Part J UX-shell tranche of Spec 0022 and the last slice of
the arc. It delivers the [Standard] long-running job progress/cancellation/timing
harness (§J.10 `JobRunner`), the [Standard] in-app help/glossary/onboarding gallery
(§J.11 `Help`), and the [Standard] 2D/3D rendered system/beam view (§J.12
`SystemView3D`) that consumes the OpenTK viewport Part A §A.6 reserved. With this
the Part J UX shell is complete; the audit-gated FuncUI clone remains UNREFERENCED
and its linking mechanism unresolved, as every prior Ui slice left it.

## What's working

- Add `JobRunner.fs` (J.10): the single shared background-job harness — a generic
  cooperative loop (`runPoints`/`runIterations`) that checks a `CancellationToken`
  BETWEEN steps, reports completed-over-total (or indeterminate iteration) progress
  with elapsed time, and runs on a thread-pool worker; its MVU surface gives a
  Start↔Cancel single button, refreshes committed results on completion, and leaves
  prior results intact on cancel (the dropped partial is the undo).
- Confine every derived artefact to the project `.sidecars` directory through the
  single `Storage.Sidecar` sidecar-location seam (reusing the Softellect `.binz`
  extension), never the repo root (J.10 item 6); `JobRunner` and the Part G fit page
  now route through one seam, not two divergent conventions.
- Add `Help.fs` (J.11): a units/material glossary stating the §A.10 eV/cm⁻¹
  conversions by CALLING `Domain.Units` (no second implementation), static field
  tooltips, and a five-entry onboarding gallery of shipped sample projects for the
  named `Analytics/Examples/*.fsx` cases, each opened through the schema-validated
  `Templates.loadTemplate` path; no `.fsx` is executed at runtime. The gallery builds
  its samples through the shared `Templates` starting-project scaffold rather than a
  forked copy.
- Add `SystemView3D.fs` (J.12): a pure system/beam geometry that places each
  `ConstructorElement` to scale along the beam path and reads each beam segment's
  direction from the already-solved `EmFieldSystem.reflected`/`.transmitted`
  (`EmField.normal`) — consuming the solved beam tree, never re-solving or
  re-routing, with conversion to viewport coords only at the render boundary.
- Wire the Ui fsproj compile order (`JobRunner` → `Help` → `SystemView3D`, after the
  J.6–J.9 modules) with NO FuncUI-clone reference, register the two new test files,
  and add the AC-J12 `SystemView3D` portion to `SchematicGeometryTests.fs`; 19 new
  tests (AC-J10/J11/J12) pass.

## Tests

- `build` gate — PASS (`dotnet build Berreman.slnx -c Release`; 0 errors; the three
  new Ui modules compile against the public MIT Avalonia/OpenTK stack; no
  FuncUI-clone reference, no new C# project).
- `unit-tests` gate (BerremanTests) — PASS, 84 passed / 5 skipped (baseline 84; not
  touched by this slice).
- `constructor-unit-tests` gate — PASS, 204 passed (185 baseline + 19 new):
  - `JobRunnerCancelTests` (AC-J10, 8 facts): `sweepTotalPoints` equals the engine's
    `0..length` inclusive count; a run reports strictly increasing completed-over-total
    progress to completion; a cancel observed between points stops within one
    inter-point check carrying only the partial; a cancel leaves prior committed
    results intact while completion refreshes them; the Start↔Cancel button shows two
    states; an unknown-total job reports an indeterminate-but-running state; derived
    artefacts resolve under the project sidecar dir (never the repo root).
  - `HelpGalleryTests` (AC-J11, 7 facts): every gallery entry opens a shipped sample
    project through the schema-validated path; the gallery covers the five named
    example cases; the EUV sample is a many-layer Mo/Si stack; the eV and cm⁻¹
    conversions reuse `Domain.Units` (matching `evNmProduct/λ` and `1e7/λ`); the
    glossary states both relations; tooltips are non-empty static strings.
  - `SchematicGeometryTests` AC-J12 portion (4 facts): a beam segment direction is
    read from the already-solved branch normal; reflected and transmitted segments
    carry independent solved directions; building the viewport geometry writes no
    value back into the model; a multi-element system places its elements to scale in
    beam-path order.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 204
```

## Architecture

- **`JobRunner` is a generic harness over an injected `step : int -> 'a`.** The spec
  frames it as the single shared background-job harness hosting BOTH sweeps and fits,
  with Part G supplying "only the per-iteration progress payload" (slice 011).
  Making it physics-agnostic CONSUMES `Variables.calculate`/`calculate3D` and the
  ALGLIB fit through the injected step rather than forking the engine's per-point
  routing or the optimizer loop (§0 constraint 2). Sweep granularity stays tied to
  `RangedVariable.length` via `sweepTotalPoints` (`Variables.fs:42`), so the engine's
  own point count drives progress.
- **Cancel drops the partial; only completion commits.** The MVU model keeps
  `committedResults` across a Start and an explicit `RunCancelled`; only
  `RunCompleted` swaps in new results (item 4 refresh / item 5 undo). The cooperative
  loop's `CancelledWith partial` outcome is the signal the partial is discardable.
- **Gallery reuses the template open seam.** `Help.openEntry` routes a literal sample
  project through `Templates.loadTemplate` (`serializeProject >> deserializeProject`,
  the validate-on-load core `ProjectFile.openProject` also runs), so §A.7 schema
  validation applies uniformly and no second deserialize is invented; the `.fsx`
  files are design-reference labels only.
- **`SystemView3D` consumes the solved beam tree.** Segment directions come from
  `EmField.normal` (the engine's Poynting unit vector) of the
  `EmFieldSystem.reflected`/`.transmitted` returned by `BeamTree.solve`; the render
  functions take the solved `EmFieldSystem` as INPUT and never call the solver, so
  the no-re-solve invariant is structural. Geometry is a pure projection (no OpenTK
  type), converting canonical meters to viewport coords only at the boundary.

- **Cycle-2 reuse consolidation (F1/F2).** The §A.7 starting-project scaffold lives
  in exactly one place: `Templates.fs`'s `air`/`glass`/`film`/`glassPlate`/
  `defaultLight`/`systemOf`/`projectOf` are module-public and `Help.fs` calls them
  (F1), so a future change to the starting-project shape is made once; the
  `defaultLight` wavelength is the single 550 nm value (the prior 600/550 fork is
  gone). The sidecar-location policy lives in `Storage.Sidecar`
  (`sidecarDirectory`/`derivedArtefactPath`), and both `JobRunner.derivedArtefactPath`
  and `SynthesisFitPage.fitHistorySidecarPath` delegate to it (F2) — the location was
  homed in Storage rather than in `JobRunner` because compile order puts the fit page
  before `JobRunner`, and Storage is where the `.binz` extension/IO already lives.
  F3 (the `Thickness`→meters fold shared by `Schematic`/`SystemView3D`) is left as-is:
  two distinct render surfaces with intentionally different scale constants.

## Deferred

- The FuncUI/OpenTK VIEW bindings for all three modules (the live `JobRunner`
  status control wired into `AppShell`'s shared status area, the help/gallery panel,
  and the OpenTK `SystemView3D` surface) follow the established view-binding-deferred
  precedent — this slice lands the harness, the help content + gallery samples, and
  the pure system/beam geometry; the Avalonia/OpenTK hosting awaits the §A.6/§A.9
  clone-audit + linking decision, which stays unresolved.
- The richer docking/redocking and floating-window chrome (the clone's to supply
  once cleared) remains deferred from slice 015.
- Full 3D vector wave optics, diffraction rendering, a second physics engine, a
  retained-mode scene graph, level-of-detail, and camera-state persistence are out
  of scope (§J.12 / Part A §A.5); the rendered segments are the straight ray/beam
  paths the orchestration already carries.
- A job queue/scheduler, retry-on-failure loops, and thread-killing are out of scope
  (§J.10 — one job per worker, cancel-and-restart); an authorable/markdown
  help-content pipeline is out of scope (§J.11).

## Gotchas

- `sweepTotalPoints x = x.length + 1`, because `Variables.calculate`/`calculate3D`
  iterate `0 .. x.length` INCLUSIVE (`Variables.fs:267,298`). A future reader wiring
  a sweep into `JobRunner.runPoints` must pass this total, not `x.length`, or the
  last engine point is dropped from the progress bar.
- `JobRunner.runPoints`/`runIterations` check the token at the TOP of each iteration,
  so a cancel requested from inside the step body stops on the NEXT iteration — the
  in-flight step always finishes (cooperative, not pre-emptive). The cancel test
  cancels inside the step and asserts exactly `cancelAfter` points ran.
- `SystemView3D.beamDirection`/`beamSegments` deliberately take the SOLVED
  `EmFieldSystem` as a parameter (not a `BeamNode`) so the renderer cannot re-solve;
  callers must `BeamTree.solve` once upstream and pass the result in.
- The two grep hits for "Avalonia.FuncUI.Clone" under `OpticalConstructor.Ui` are
  documentation COMMENTS (in the Ui fsproj and `AppShell.fs`) asserting the clone
  stays unreferenced — there is no `PackageReference`/`ProjectReference` to it.
- xUnit `Assert.Equal(a, b, digits)` requires `digits` in 0..15; the `Assert.NotEqual`
  over two strings needs an explicit `<string>` to disambiguate the seq overload.
- The shared sidecar seam was homed in `Storage.Sidecar`, NOT in `JobRunner`, because
  `SynthesisFitPage.fs` compiles BEFORE `JobRunner.fs` (Ui fsproj lines 33 vs 61) and
  the slice mandates `JobRunner` be registered last — so the fit page cannot call
  `JobRunner`. A future reader adding a new derived-artefact writer should call
  `Storage.Sidecar.derivedArtefactPath`, not re-home the policy in a Ui module.
- The fit-history sidecar now lands under `workingFolder/.sidecars/` (was flat
  `workingFolder/`); still under the project working folder, never the repo root. No
  test asserted the old flat path, so the move is behaviour-preserving for the gates.

## Changelog

- 2026-06-01 — 016 (LAST): final Part J UX tranche — `JobRunner.fs` (J.10) the
  shared background sweep/fit harness with cooperative cancellation, completed-points/
  iteration progress, elapsed timing, the Start↔Cancel toggle, MVU completion
  refresh, and sidecar-only derived artefacts; `Help.fs` (J.11) the units/material
  glossary reusing `Domain.Units` for the §A.10 eV/cm⁻¹ conversions, static field
  tooltips, and the `Analytics/Examples/*.fsx`-sourced onboarding gallery opened
  through the schema-validated path (no `.fsx` executed); `SystemView3D.fs` (J.12)
  the reserved OpenTK system/beam viewport geometry consuming the already-solved
  beam tree (no re-solve, no non-SI write-back). Clone unreferenced, linking
  unresolved. Gates green (build; unit-tests 84; constructor-unit-tests 204).
- 2026-06-01 — 016 cycle-2 retry: resolved reuse findings F1/F2. F1 — lifted the
  shared §A.7 starting-project scaffold (`air`/`glass`/`film`/`glassPlate`/
  `defaultLight`/`systemOf`/`projectOf`) to module-public in `Templates.fs` and had
  `Help.fs` call it, removing the verbatim clone and the 600-vs-550 nm `defaultLight`
  drift (single value: 550 nm). F2 — added the single sidecar-location seam to
  `Storage.Sidecar` and routed `JobRunner.derivedArtefactPath` and
  `SynthesisFitPage.fitHistorySidecarPath` through it. F3 left as-is per the retry
  hint. Pure reuse refactor; gates re-confirmed green (build; unit-tests 84;
  constructor-unit-tests 204), clone still unreferenced.
