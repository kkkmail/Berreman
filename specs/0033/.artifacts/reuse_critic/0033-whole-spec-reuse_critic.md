# Reuse critique -- .spec-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (single root)
- Files inspected: 200/200 -- **cap tripped** mid-`specs/0022/`; the `.md/.json` census is dominated by `specs/00xx` process-trail artifacts plus the Storage/Ui schema JSONs; no `.py` files exist.
- Extensions: `.py,.md,.json` per bounds. The bounds exclude `.fs` in an F# repo, so every code-level claim below was verified by targeted reads of the exact files the spec cites; this deviation is recorded here so the judge can weigh it.

Reviewed artifact: the whole-spec bundle `specs/0033/` (`.spec-md` + `.spec-jsonl`). The current `git diff HEAD` contains only the claim-check wording fix to `.spec-md` (§A.0 "only catalogue proxy") plus `.history`/`cost.json` ledger entries -- **no code symbols** -- so findings concern duplication the *plan* would create, not code already written. A prior draft of this critique existed at this path from before the spec-cycle commit; this run re-verified its findings against the repo and supersedes it. Spot-verified citations all land exactly (`LibraryProxy` `ElementId.fs:179-184`, `createInMemory` `:359`, `ExperimentProxy` `:665-693`, `MaterialImport.fs:109-127`, `DispersionModels.fs:120,156-185`, `Validation.fs:46,56,64,92`, `RepeatBuilder.fs:24`, `SeriesData.fs:103-104`, `Geometry.fs:578-622`, engine `Dispersion.fs:9,24,38,53`).

## Findings

### F1: RII formula-1 import keeps its hand-rolled Sellmeier while step 25 lowers formulas 2-7 through `DispersionModel`

- **Worker added (planned):** step 25 extends `importRefractiveIndexInfo` with formulas 2-7 "each lowered through a DispersionModel or SumOfTerms to an EpsAxisDispersion ... (so imports are editable)", but leaves the existing formula-1 branch untouched.
- **Existing helper:** the formula-1 branch already hand-rolls an inline Sellmeier closure at `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs:120-127`, duplicating the physics `DispersionModels.Sellmeier`/`baseIndex` already implement at `OpticalConstructor.Domain/DispersionModels.fs:156-162`.
- **Why it matters:** RII formula 2 *is* Sellmeier (formula 1 with unsquared C). After step 25 the same function would contain two Sellmeier implementations side by side -- formula 1 via the legacy inline closure, formula 2 via the new lowering. Worse, formula-1 imports would stay closure-backed with `complexity = None`, i.e. **not editable**, contradicting step 25's own stated goal for the formula that dominates the RII database.
- **Suggested action:** extend step 25 to re-point the existing formula-1 branch through the same `DispersionModel`/`toEpsAxis` lowering as formulas 2-7, so Sellmeier physics lives once and formula-1 imports gain an editable `MaterialComplexity`.

### F2: The Ui validation rules get second homes instead of the move the steering doc mandates

- **Worker added (planned):** step 21 (Domain) has `SampleStackEditor.SetRepeatCount` "rejecting count < 1 with a typed error (the same rule Validation.validateRepeatCount enforces at the Ui boundary, OpticalConstructor.Ui/Validation.fs:56)"; step 23 (TestWindows) surfaces "the imaginaryIndexGainWarning rule, OpticalConstructor.Ui/Validation.fs:92".
- **Existing helper:** `OpticalConstructor.Ui/Validation.fs` -- `validateThickness:46`, `validateRepeatCount:56`, `validateWavelengthRange:64`, `imaginaryIndexGainWarning:92`.
- **Why it matters:** TestWindows references only Domain and Controls (step 19's own note re `TestWindows.fsproj:79,81`), and Domain cannot reference Ui, so the cited helpers are *unreachable* from where steps 21/23 need them. The steering `.spec-md` Part F resolves this correctly -- "these helpers move with the editors where project layering requires, never duplicated" -- but **no step in `.spec-jsonl` owns that move** (contrast step 19, which explicitly owns the real-move of the `MaterialPreview` spectral-axis helpers). A worker executing step 21 or 23 as written has no path except re-implementing the rule, and "the same rule ... enforces at the Ui boundary" reads as an invitation to do exactly that.
- **Suggested action:** assign the real-move of the needed `Validation.fs` subset (`ValidationError`/severity shape, `validateRepeatCount`, `validateThickness`, `imaginaryIndexGainWarning`) into Domain to a concrete step (21 is the natural owner), with Ui re-pointed -- mirroring how step 19 handles the spectral-axis helpers.

### F3: Analytic dispersion physics becomes double-encoded (formula in `baseIndex`, again as term data in `toEpsAxis`)

- **Worker added (planned):** step 11's `toEpsAxis` re-derives Cauchy, Sellmeier, ConstantNK, Lorentz and Drude as `DispersionTerm` lists; step 12 adds Forouhi-Bloomer and Brendel-Bormann to *both* the `baseIndex` dispatch and the lowering.
- **Existing helper:** the analytic implementations already live in the single `baseIndex` dispatch at `OpticalConstructor.Domain/DispersionModels.fs:156-185`, with `evaluate` at `:206`.
- **Why it matters:** after step 11 each finite-sum model's physics exists twice -- once analytically in `baseIndex`, once term-encoded in `toEpsAxis` -- and every future model must be written in both places. The spec's own mitigation (grid-equality tests, AC-B5) proves the duplication is real: those tests exist precisely to keep two encodings of one formula from drifting. Notably the spec already contains the single-source pattern for half the catalogue: TaucLorentz/GaussianOscillator lower as wrappers over `evaluate` rather than re-encoding.
- **Suggested action:** consider making the term data the single source for the finite-sum models (`baseIndex` delegates through the lowering, or the lowering becomes the only implementation); otherwise leave as-is with the AC-B5 grid tests carrying the divergence risk, but record that choice explicitly.

### F4: Mutable-closure write proxies diverge from the repo's existing editable-collection pattern

- **Worker added (planned):** steps 5/6 (`SampleProxy.createInMemory` / `MaterialProxy.createInMemory`) close over a `ref Map` and mutate inside the closure, citing the read-only `LibraryProxy` (`ElementId.fs:179-184`) as the shape precedent.
- **Existing helper:** the repo's only existing precedent for an *editable* collection is the pure `Experiments.ExperimentCollection` (`ElementId.fs:537-653`; pure state transitions such as `setRangeMin`/`setRangeMax` at `:586-592`); the existing proxies (`LibraryProxy` `:179`/`createInMemory :359`, `ExperimentProxy` `:665`/`createInMemory :688`) are read-only seed seams.
- **Why it matters:** the spec cites `LibraryProxy` for the record-of-functions *shape*, which is accurate, but the *behaviour* being added (add/update/remove with typed rejections) is solved elsewhere in the same file by pure collection functions the host owns, unit-tested with zero mutable state. Two idioms for the same job will coexist in one file's neighbourhood, and future workers must guess which to copy.
- **Suggested action:** likely leave as-is -- constraint 0.3 is a recorded decision and the stateful proxy is a deliberate write-seam for a future disk-backed store, which the pure-collection shape does not model -- but the spec should say *why* the `ExperimentCollection` precedent was not followed, so the divergence is documented rather than accidental.

### F5: Second period-expansion primitive "mirroring" `RepeatBuilder.expand`

- **Worker added (planned):** step 1 gives `SampleStructure` a pure expansion member mirroring `RepeatBuilder.expand`.
- **Existing helper:** `OpticalConstructor.Ui/RepeatBuilder.fs:24-25` -- `expand` is exactly `List.replicate count cell |> List.concat` over engine `Layer`s (the body is already fully generic; only the annotation pins it to `Layer`).
- **Why it matters:** low severity -- one line, and Domain cannot reach Ui, so literal reuse is impossible in the planned direction. Still, the repo's tests treat duplicated expansion loops as a defect (`TemplatesTests.fs:14`: films must be "exactly RepeatBuilder.expand output (not a duplicated loop)"), and after step 1 the two modules' docs contradict each other: `RepeatBuilder.fs:1-8` records "NO new super-layer/period domain type", which this spec's recorded decision reverses.
- **Suggested action:** either generalize the expansion in Domain (`'a list -> int -> 'a list`) and re-point `RepeatBuilder.expand` to it in a later step, or accept the one-line duplication and update `RepeatBuilder`'s module doc so the docs stop contradicting each other.

### F6: Step 19's n/k series builders parallel the existing `SeriesData` wrappers

- **Worker added (planned):** a pure `nkDispersionChart` in TestWindows wrapping `Analytics.Variables` n/k calculators into shared-chart series.
- **Existing helper:** `OpticalConstructor.Ui/Charts/SeriesData.fs:103-104` (`plotN11Series`/`plotXi11Series`) already wraps the same builders into `Series1D` values.
- **Why it matters:** minor -- the heavy lifting stays shared in `Analytics.Variables`, and the two wrappers target different chart record types, so only the thin naming/labelling layer duplicates (axis-label conventions can drift between the Ui charts and the new dual-axis view).
- **Suggested action:** since step 17 moves the chart model into Controls and Domain already references Analytics (see the `RangedVariable` reuse note at `Domain/SourceSpec.fs:100-103`), consider hosting one set of n/k series builders where both hosts can reach them; otherwise leave as-is -- the cost is cosmetic.

### F7: Step 15's `[<Literal>]` UiIds prescription diverges from all eight existing Controls siblings

- **Worker added (planned):** steps 15/16 mandate "one `[<RequireQualifiedAccess>] module UiIds` of `[<Literal>]` ids" for `MaterialsControls`/`SampleLibraryControls`.
- **Existing helper:** the eight existing `UiIds` modules in `OpticalConstructor.Controls` -- `LibraryControls.fs:77`, `ElementPaletteControls.fs:42`, `ExperimentControls.fs:193`, `LayerBandsControls.fs:43`, `RayPositionControls.fs:41`, `RendererControls.fs:112`, `Ribbon.fs:40`, `RotationControls.fs:61` -- all use plain `let` bindings, including function-valued row ids (`LibraryControls.UiIds.entry : string -> string`, `:82`) that *cannot* be `[<Literal>]`.
- **Why it matters:** the spec's model file is `LibraryControls` itself ("copying the domain-free LibraryControls shape"), yet its letter prescribes an annotation no sibling uses. A worker following it literally produces the only two `[<Literal>]`-annotated UiIds modules in the project and hits a wall at the first parameterised row id (both new bays list entries, so they will need one). CLAUDE.md's `[<Literal>]` wording is the source, but the uniform in-tree precedent deviates deliberately. The claim-checker noted the same mismatch inside its confirmed claim 22.
- **Suggested action:** reuse the sibling convention (plain `let`, `[<RequireQualifiedAccess>]`, intent-named), applying `[<Literal>]` only where an id is genuinely constant if the judge wants CLAUDE.md's letter honoured; the two new modules should match their eight siblings either way.

### F8: Step 7's `WaveLengthInterval` has an exact-shape twin in Ui

- **Worker added (planned):** `WaveLengthInterval (lower : WaveLength, upper : WaveLength)` in core `Dispersion.fs` (step 7).
- **Existing helper:** bounds-only `WavelengthRange = { min : float<meter>; max : float<meter> }` at `OpticalConstructor.Ui/UserEnvironment.fs:83-88`; distinct from the *sampled* `Range<'T>`/`WaveLengthRange` sweep descriptors at `Analytics/Variables.fs:22-29,46`, which do not apply here (they carry a point count).
- **Why it matters:** low severity -- the core engine sits below both Ui and Analytics, so it cannot reuse either type, and a new core type is the defensible choice. But the repo will then hold two bounds-only wavelength-pair types with near-identical names and no cross-reference, in a codebase whose own comments police exactly this ("no fresh `Range<_>`", `SourceEditorView.fs:79`; `SourceSpec.fs:100-103`).
- **Suggested action:** leave the new core type as planned, but record the relationship (validity interval vs preference default) and note that Ui's `WavelengthRange` could later be re-expressed via the engine type -- preventing a third bounds-pair from appearing unacknowledged.

## Bottom line

This spec is unusually reuse-disciplined -- every verified citation lands exactly, it deletes existing duplication (the `Propagation.fs` string-branching and glass fallback), and it repeatedly names the exact constructor to build through. The substantive items are F1 and F2, both cheap spec-text fixes (one sentence in step 25; one ownership line in step 21) and both likely to produce real divergent duplicates if executed as written; F3 is a deliberate-looking trade-off the judge should confirm was chosen rather than stumbled into, and F4-F8 are documentation/convention-alignment advisories. My read: worth a spec-text revision pass before implementation starts, not a re-spawn of the whole cycle -- the judge decides.
