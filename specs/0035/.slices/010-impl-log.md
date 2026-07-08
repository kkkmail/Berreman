# Impl-log — slice 010 (IMPLEMENT: dispersive gyration ρ and Polder μ editing)

## Progress

- [x] Read system/project/slice prompts, the arc context (spec 0035 Part C),
  and every consumer of the `MaterialComplexityEditor` public API.
- [x] Extended `MaterialComplexityEditor.fs` (state, messages, apply, toComplexity, ofComplexity).
- [x] Fixed the forced `editErrorReason` OR-pattern in `MaterialEditorView.fs`.
- [x] Added 4 acceptance tests to `MaterialComplexityTests.fs`.
- [x] Local due-diligence build + tests (advisory; the arc-runner is the gate authority).
- [x] Wrote the state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs` — the primary change:
  - New `ComponentDispersion` DU (`ConstantComponents | DispersiveComponents`).
  - `MaterialComplexityEditState` gains `activityDispersion`, `gyrationDispersion`,
    `magneticDispersion`, `polderDispersion` (dispersive facets stored INDEPENDENTLY
    of the constant facets — lossless uncheck). `defaultState` seeds them.
  - DELETED `UnsupportedComplexity` from `MaterialComplexityEditError` (§0.2).
  - New messages: `SetActivityDispersion`, `SetGyrationComponentDispersion`,
    `SetMagneticDispersion`, `SetMuDiagonalDispersion`, `SetMuParallelDispersion`,
    `SetMuGyrationDispersion`.
  - `gyrationComponents` / `setGyrationComponent` generalised to `GyrationClass<'g>`
    (the constant call sites keep `'g = RhoValue`; the dispersive facet reuses them
    with `'g = DispersionFormula`).
  - `defaultGyrationFormula`, `defaultPolderDispersion`, `syncGyrationDispersion` (class-sync).
  - `applyMaterialComplexityMsg`: `ChooseAnisotropy` / `SetActivity ActivityOn` /
    `ChooseGyrationClass` now keep `gyrationDispersion` class-synced; `ChooseGyrationAxis`
    sets the axis on BOTH Polder facets; 6 new arms added.
  - `toComplexity`: the active + magnetic branches honour the sub-toggle —
    `ConstantComponents` → `RhoWithoutDispValue` / `MuWithoutDispValue`,
    `DispersiveComponents` → `RhoWithDispValue` / `MuWithDispValue`.
  - `ofComplexity`: seeds a dispersive ρ (`RhoWithDispValue`) / μ (`MuWithDispValue`)
    verbatim into the dispersive facet (the constant facet takes the class's default,
    class-synced); the `UnsupportedComplexity` error paths are gone. Seeding never fails.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs` — forced
  1-line edit: `UnsupportedComplexity` removed from the `editErrorReason` OR-pattern
  (unavoidable consequence of deleting the DU case; the view otherwise untouched).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs` — 4 new tests.

## Testing state

Per the IMPLEMENT worker role (Invariant 6 — the worker acts and runs no checks),
the `build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests` gates
are executed by the arc-runner's deterministic gate engine AFTER this worker exits;
they were NOT run here as authoritative gates. The results below were observed LOCALLY
as due diligence (advisory only), matching the slice-009 precedent:

- `build Berreman.slnx -c Release`: **0 errors**; no `FS####` warning originates in my
  three files (verified by `--no-incremental` rebuilds grepped per file). The only
  remaining warnings are pre-existing and outside this slice's scope: `MSB3277`
  (WindowsBase conflict from the Ui WebView2 chain — introduced before this arc, per the
  step-008/009 SoWs; this slice changed no project references) and `FS1125` in
  `SeriesDataTests.fs` (not my file).
- `unit-tests` (BerremanTests): 119 passed / 5 skipped (baseline held; no core file changed).
- `constructor-unit-tests`: **444 passed** (440 → 444; the 4 new Part-C tests).
- `ui-smoke`: 93 passed (baseline held). `ui-tests`: 327 passed (baseline held).
- `MaterialEditorWindowTests` (the round-trip + editor window over the changed Domain):
  31 passed — the constant built-in round-trip and lossless-uncheck tests are intact.
- No CRLF churn (`git diff --numstat` identical to `--ignore-cr-at-eol`).

## Artifacts

None (pure Domain + tests; no captured logs/screenshots/traces produced).

## Gotchas

- **`DispersionFormula`, not a full `DispersionModel`, per component.** The step's
  `how_to` says "a dispersive DispersionModel"; the authoritative spec §C.0 says the
  component "becomes a `DispersionFormula` edited through the SAME `DispersionModels`
  `modelParameters` + raw `SumOfTerms` surface." A gyration/Polder component is a REAL
  SCALAR, and `RhoWithDispValue`/`MuWithDispValue` consume `GyrationClass<DispersionFormula>`
  / `PolderValue<DispersionFormula>` DIRECTLY — so storing the `DispersionFormula` is the
  exact, lossless representation and the raw `SumOfTerms (RealNK …)` payload the 0033
  surface edits. A general `DispersionModel -> DispersionFormula` is impossible for the
  transcendental / ComplexEps models, which would reintroduce the very error path §0.2
  removes. I followed §C.0.
- **Parallel-field, not a combined `GyrationClass<edit>`.** The CONSTANT fields
  (`gyration : GyrationClass<RhoValue>`, `polder : PolderValue<MuValue>`) are UNTOUCHED,
  so the `MaterialEditorView` constant panels (`gyrationPanel`/`muPanel`, which read
  `m.editor.gyration`/`.polder`) and every existing test compile and pass verbatim. The
  DISPERSIVE facets are new fields the view does not read yet (the dispersive editing UI
  is a future Ui slice — Ui is outside this slice's `touches`).
- **Class-sync invariant.** `gyration` (constant) and `gyrationDispersion` (dispersive)
  must share the symmetry class (one class picker, driven by `gyration`).
  `syncGyrationDispersion` enforces it at the 3 mutation points; a class change resets the
  dispersive formulas exactly as it resets the constant magnitudes; a same-class re-pick
  keeps them (lossless). The round-trip test pins it.
- **Dispersive μ is always the full Polder tensor.** The engine's `MuWithDispValue` has no
  scalar dispersive case, so `toComplexity`'s `DispersiveComponents` magnetic branch emits
  `MuWithDispValue polderDispersion` regardless of `muKind` (which gates only the constant
  branch). The magnetization axis is a single physical choice, so `ChooseGyrationAxis` sets
  it on BOTH facets.
- **Future Ui note (not this slice):** the dispersive component number boxes, when the view
  is wired, must commit on `LostFocus` (not auto-commit) to avoid the FuncUI render-loop
  hang — same discipline the existing coefficient/component boxes already use.
- **MSB3277 is pre-existing** and not owned by this slice (no project reference changed).
