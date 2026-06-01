# 016 impl-log — UX shell III (Part J §J.10–J.12)

## Progress

- [x] Read system prompt, project prompt, slice spec, gates, prior SoW.
- [x] Surveyed existing modules (Schematic, BeamTree, SynthesisFitPage, Templates,
      Units, Sidecar, ProjectFile, Variables, Fields/EmField).
- [x] Wrote impl-plan.
- [x] R-1 `JobRunner.fs` (background harness: progress/cancel/timing, MVU, sidecar).
- [x] R-2 `Help.fs` (glossary reusing Units, tooltips, gallery via openProject path).
- [x] R-3 `SystemView3D.fs` (pure system/beam geometry reading solved EmField dirs).
- [x] `JobRunnerCancelTests.fs` (AC-J10).
- [x] `SchematicGeometryTests.fs` AC-J12 portion + Help gallery facts.
- [x] Register modules in the two fsprojs.
- [x] Run gates: build, unit-tests, constructor-unit-tests.
- [x] **Cycle-2 retry: resolved reuse findings F1 (verbatim scaffold duplication)
      and F2 (divergent sidecar-location seam).**

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Ui/JobRunner.fs`
- `OpticalConstructor/OpticalConstructor.Ui/Help.fs`
- `OpticalConstructor/OpticalConstructor.Ui/SystemView3D.fs`
- `OpticalConstructor/OpticalConstructor.Tests/JobRunnerCancelTests.fs`

Edited:
- `OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `OpticalConstructor/OpticalConstructor.Tests/SchematicGeometryTests.fs`
- `OpticalConstructor/OpticalConstructor.Tests/HelpGalleryTests.fs` (new — AC-J11)

Edited (cycle-2 retry — F1/F2):
- `OpticalConstructor/OpticalConstructor.Ui/Templates.fs` — promoted the shared
  §A.7 scaffold (`air`, `glass`, `film`, `glassPlate`, `defaultLight`, `systemOf`,
  `projectOf`) from `private` to module-public so `Help.fs` reuses it (F1).
- `OpticalConstructor/OpticalConstructor.Ui/Help.fs` — deleted the duplicated
  scaffold block; gallery builders now call `Templates.*`; gallery inherits the
  single 550 nm `defaultLight` (F1).
- `OpticalConstructor/OpticalConstructor.Storage/Sidecar.fs` — added the single
  `sidecarDirectory`/`derivedArtefactPath` sidecar-location seam (F2).
- `OpticalConstructor/OpticalConstructor.Ui/JobRunner.fs` — `derivedArtefactPath`/
  `sidecarDirectory` now delegate to `Storage.Sidecar` (F2).
- `OpticalConstructor/OpticalConstructor.Ui/SynthesisFitPage.fs` —
  `fitHistorySidecarPath` now delegates to `Storage.Sidecar.derivedArtefactPath` (F2).

## Decisions

- **JobRunner is a generic harness over an injected `step : int -> 'a`.** The spec
  frames it as "the single shared background-job harness" hosting both sweeps and
  fits, with Part G supplying "only the per-iteration progress payload". Making it
  physics-agnostic CONSUMES the engine/optimizer through the injected step instead
  of forking `Variables.calculate`'s per-point routing or the ALGLIB loop
  (§0 constraint 2). Sweep granularity stays tied to `RangedVariable.length` via
  `sweepTotalPoints` so the point count still drives progress (R-1 item 2).
- **`committedResults` survive a cancel; only completion refreshes them.** The
  cancelled job's partial output is dropped (`CancelledWith`), which is the natural
  undo (item 5); `RunCompleted` swaps in the new results (item 4).
- Gallery samples are engine-preset literals corresponding to the named `.fsx`
  cases, opened through `serializeProject >> deserializeProject` (the validate-on-
  load core `ProjectFile.openProject` runs), reusing the `Templates.loadTemplate`
  precedent; no `.fsx` executes at runtime.
- SystemView3D reads each beam segment direction from `EmField.normal` (the solved
  Poynting unit vector) of the `EmFieldSystem.reflected`/`.transmitted` returned by
  `BeamTree.solve` — consume, never re-solve.

## Cycle-2 retry (reuse findings F1 / F2)

The cycle-1 code-judge routed back on reuse finding **F1** and asked me to also
weigh **F2**; **F3** was left as-is per the retry hint. Resolution:

- **F1 — eliminated the verbatim scaffold duplication.** `Help.fs:113-149` had
  cloned seven starting-project helpers (`air`, `glass`, `film`, `glassPlate`,
  `defaultLight`, `systemOf`, `projectOf`) line-for-line from `Templates.fs`,
  including a silent `defaultLight` drift (600 nm in `Help` vs 550 nm in
  `Templates`). I promoted those helpers from `private` to module-public in
  `Templates.fs` (the canonical §A.7 starting-project scaffold's natural home;
  `highIndex`/`lowIndex` stay private — the gallery doesn't use them) and rewrote
  `Help.fs`'s gallery builders to call `Templates.film`/`systemOf`/`projectOf`/
  `glassPlate`/`glass`/`air`. `Help.fs` now keeps ONLY the gallery-specific stacks,
  the `sourceScript` labels, and `GalleryEntry`. The 600/550 divergence is resolved
  to the single `Templates.defaultLight = 550 nm`; the gallery samples now inherit it.
- **F2 — unified the sidecar-location seam.** `JobRunner.derivedArtefactPath`
  (`.sidecars/` subfolder + `Sidecar.extension`) diverged from
  `SynthesisFitPage.fitHistorySidecarPath` (flat `workingFolder` + hand-written
  `.fit-history.binz`). The reuse critic's literal suggestion — have
  `SynthesisFitPage` call `JobRunner.derivedArtefactPath` — is blocked by compile
  order: `SynthesisFitPage.fs` compiles *before* `JobRunner.fs` (Ui fsproj lines
  33 vs 61), and the slice mandates `JobRunner` be registered last. The principled
  single home for sidecar-location policy is therefore the `Storage.Sidecar` module,
  where `extension`/`writeSidecar`/`readSidecar` already live and which BOTH Ui
  consumers reference. I added `Sidecar.sidecarDirectory` and
  `Sidecar.derivedArtefactPath` there, and made `JobRunner.derivedArtefactPath`
  *and* `SynthesisFitPage.fitHistorySidecarPath` delegate to it. Both now agree on
  the `.sidecars` subfolder and the drift-proof `.binz` extension, decided once. The
  fit-history path moves from `workingFolder/<name>.fit-history.binz` to
  `workingFolder/.sidecars/<name>.fit-history.binz` — still under the project
  working folder (never the repo root, §G.10 item 6 / §0 constraint 4), and no test
  asserted the old flat shape (`SynthesisFitPageTests.fs` does not touch the path).
- **F3 — left as-is** (the `Thickness`→meters decomposition shared between
  `Schematic.fs` and `SystemView3D.fs`), as the retry hint permits; the two are
  genuinely distinct render surfaces with intentionally different pixels-per-meter
  constants.

## Testing state

All three gates pass locally (commit-ready). Re-confirmed after the cycle-2
F1/F2 consolidation:

- `build` — PASS (`dotnet build Berreman.slnx -c Release`, cwd `Berreman/`; 0 errors).
- `unit-tests` — PASS, 84 passed / 5 skipped (baseline 84; untouched here).
- `constructor-unit-tests` — PASS, 204 passed (185 baseline + 19 new:
  `JobRunnerCancelTests` 8 (AC-J10), `HelpGalleryTests` 7 (AC-J11),
  `SchematicGeometryTests` AC-J12 portion 4). Count unchanged by the F1/F2
  consolidation — it was a pure reuse refactor, not a behaviour change; every
  prior test (including the `JobRunnerCancelTests` sidecar-path assertion) stays
  green because the delegators preserve their results.

No blockers. FuncUI clone confirmed UNREFERENCED in the Ui fsproj (the single grep
hit is a documentation comment only).

## Artifacts

`specs/0022/.artifacts/016-build.log` — captured gate outcomes.
