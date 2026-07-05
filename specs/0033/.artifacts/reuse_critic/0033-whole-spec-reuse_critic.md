# Reuse critique -- .spec-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (single root)
- Files inspected: 200/200 -- **cap tripped**; the `.md/.json` census is dominated by `specs/00xx` process-trail artifacts (prior critiques, impl logs), plus the Storage/Ui schema JSONs. No `.py` files exist.
- Extensions: `.py,.md,.json` per bounds. The bounds exclude `.fs` in an F# repo, so every code-level claim below was verified by targeted reads of the exact files the spec cites (permitted by the read order's "references the slice spec names" allowance); this deviation is recorded here so the judge can weigh it.

Reviewed artifact: the whole-spec bundle `specs/0033/` (`.spec-md`, `.spec-jsonl`, `.contracts-json`) -- `git diff HEAD` is empty; the spec files are the untracked "diff". Findings therefore concern duplication the *plan* would create, not code already written. Roughly twenty of the spec's `path:line` citations to existing helpers were spot-verified and all checked out exactly (LibraryProxy `ElementId.fs:179-184`, seeds `:189`, `MaterialLibrary.fs:21,33,42-43,53,57,69,79`, `DispersionModels.fs:120,156,206,216,228,242,259`, `Validation.fs:46,56,64,92`, `RepeatBuilder.fs:24`, `Active.fs:30,38,46,54`, `MaterialPreview.fs:18,31,40`, `SeriesData.fs:103-104`, engine `Dispersion.fs:9,24,38,53`).

## Findings

### F1: RII formula-1 import keeps its hand-rolled Sellmeier while step 25 lowers formulas 2-7 through `DispersionModel`

- **Worker added (planned):** step 25 extends `importRefractiveIndexInfo` with formulas 2-7 "each lowered through a DispersionModel or SumOfTerms to an EpsAxisDispersion ... so imports are editable", but leaves the existing formula-1 branch untouched.
- **Existing helper:** the formula-1 branch already hand-rolls an inline Sellmeier closure at `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs:122-126`, duplicating the physics that `DispersionModels.Sellmeier` / `baseIndex` already implement at `OpticalConstructor.Domain/DispersionModels.fs:121,159-162` (the RII `c0` offset folds in as a degenerate oscillator with `C = 0`, i.e. `B*λ²/(λ²−0) = B`).
- **Why it matters:** RII formula 2 *is* Sellmeier (formula 1 with unsquared C). After step 25 the same function would contain two Sellmeier implementations side by side -- formula 1 via the legacy inline closure, formula 2 via the new lowering. Worse, formula-1 imports would stay closure-backed with `complexity = None`, i.e. **not editable**, directly contradicting step 25's own stated goal ("so imports are editable") for the one formula that dominates the RII database.
- **Suggested action:** extend step 25 to re-point the existing formula-1 branch through the same `DispersionModel`/`toEpsAxis` lowering as formulas 2-7, so Sellmeier physics lives once and formula-1 imports gain an editable `MaterialComplexity`.

### F2: The Ui validation rules get second homes instead of the move the steering doc mandates

- **Worker added (planned):** step 21 has `SampleStackEditor.SetRepeatCount` "rejecting count < 1 with a typed error (the same rule Validation.validateRepeatCount enforces at the Ui boundary)"; steps 22/23 invoke `validateThickness`-shaped rules and "the imaginaryIndexGainWarning rule" from editor windows in `OpticalConstructor.TestWindows`.
- **Existing helper:** `OpticalConstructor.Ui/Validation.fs` -- `validateThickness:46`, `validateRepeatCount:56-58`, `validateWavelengthRange:64`, `imaginaryIndexGainWarning:92-95`.
- **Why it matters:** TestWindows references only Domain and Controls (step 19's own note re `TestWindows.fsproj:79,81`), so the cited Ui helpers are *unreachable* from where steps 22/23 need them, and Domain (step 21) cannot reference Ui either. The steering `.spec-md` Part F resolves this correctly -- "these helpers move with the editors where project layering requires, never duplicated" -- but **no step in `.spec-jsonl` owns that move** (contrast step 19, which explicitly owns the real-move of the `MaterialPreview` spectral-axis helpers). A worker executing step 21 or 23 as written has no path except re-implementing the rule, and the "same rule ... enforces at the Ui boundary" phrasing in step 21 reads as an invitation to do exactly that.
- **Suggested action:** assign the real-move of `Validation.fs` (or the needed subset: `ValidationError`/`Severity`, `validateRepeatCount`, `validateThickness`, `imaginaryIndexGainWarning`) into Domain to a concrete step (21 is the natural owner), with Ui re-pointed -- mirroring how step 19 handles the spectral-axis helpers.

### F3: Analytic dispersion physics becomes double-encoded (formula in `baseIndex`, again as term data in `toEpsAxis`)

- **Worker added (planned):** step 11's `toEpsAxis` re-derives Cauchy, Sellmeier, ConstantNK, Lorentz and Drude as `DispersionTerm` lists / inverse terms; step 12 adds two more models to *both* the `baseIndex` dispatch and the lowering.
- **Existing helper:** the analytic implementations already live in the single `baseIndex` dispatch at `OpticalConstructor.Domain/DispersionModels.fs:156-185` with `evaluate:206`.
- **Why it matters:** after step 11 each finite-sum model's physics exists twice -- once analytically in `baseIndex`, once term-encoded in `toEpsAxis` -- and every future model must be written in both places. The spec's own mitigation (grid-equality tests, AC-B5) proves the duplication is real: the tests exist precisely to keep two encodings of the same formula from drifting. Notably, TaucLorentz/GaussianOscillator avoid the double-encoding by lowering as wrappers over `evaluate`, so the spec already contains the single-source pattern for half the catalogue.
- **Suggested action:** consider making the term data the single source for the finite-sum models -- `baseIndex` delegates through `toEpsAxis`'s lowering (or the lowering becomes the only implementation) -- so a formula is written once; otherwise leave as-is and let the AC-B5 grid tests carry the divergence risk, but record that choice explicitly.

### F4: Mutable-closure write proxies diverge from the repo's existing editable-collection pattern

- **Worker added (planned):** steps 5/6 (`SampleProxy.createInMemory` / `MaterialProxy.createInMemory`) close over a `ref Map` and mutate inside the closure, citing the read-only `LibraryProxy` (`ElementId.fs:179-184`) as the shape precedent.
- **Existing helper:** the repo's only existing precedent for an *editable* collection is the pure `Experiments.ExperimentCollection` (`ElementId.fs:537-653`: `commit:599`, `edit:634`, `remove:650` are pure state transitions; the proxies `LibraryProxy` and `ExperimentProxy` at `:665-693` are read-only seeds/list seams).
- **Why it matters:** the spec cites `LibraryProxy` for the record-of-functions *shape*, which is accurate, but the *behaviour* being added (add/update/remove with typed rejections) is solved elsewhere in the repo by pure collection functions the host owns, unit-tested with zero mutable state. Two idioms for the same job will now coexist in one file's neighbourhood, and future workers must guess which to copy.
- **Suggested action:** likely leave as-is -- constraint 0.3 is a recorded decision and the stateful proxy is a deliberate write-seam for a future disk-backed store, which the pure-collection shape does not model -- but the spec should say *why* the `ExperimentCollection` precedent was not followed, so the divergence is documented rather than accidental.

### F5: Second period-expansion primitive "mirroring" `RepeatBuilder.expand`

- **Worker added (planned):** step 1 gives `SampleStructure` "a pure expansion member mirroring RepeatBuilder.expand".
- **Existing helper:** `OpticalConstructor.Ui/RepeatBuilder.fs:24-25` -- `expand` is exactly `List.replicate count cell |> List.concat` over engine `Layer`s.
- **Why it matters:** low severity -- the helper is one line, monomorphic on `Layer`, and lives in Ui where Domain cannot reach it, so literal reuse is impossible in the planned direction. Still, after step 1 the repo holds two period-expansion primitives whose module docs disagree about whether periods are first-class (the `RepeatBuilder` doc's "NO new super-layer/period domain type" is reversed by this spec's recorded decision).
- **Suggested action:** either generalize the expansion in Domain (`'a list -> int -> 'a list`) and re-point `RepeatBuilder.expand` to it in a later step, or accept the one-line duplication and update `RepeatBuilder`'s module doc so the two docs stop contradicting each other.

### F6: Step 19's n/k series builders parallel the existing `SeriesData` wrappers

- **Worker added (planned):** `nkDispersionChart` in TestWindows wraps `Analytics.Variables.calculateN11Re` / `calculateXi11Im` into shared-chart series.
- **Existing helper:** `OpticalConstructor.Ui/Charts/SeriesData.fs:100-111` already wraps the same builders (`plotN11Series:103`, `plotXi11Series:104`) into `Series1D` values.
- **Why it matters:** minor -- the heavy lifting (the n/k formulas) stays shared in `Analytics.Variables`, and the two wrappers target different chart record types, so only the thin naming/labelling layer duplicates ("Re[e11]"/"Im[e11]" vs "n"/"k" conventions can drift between the Ui charts and the new dual-axis view).
- **Suggested action:** since step 17 already moves the chart model into Controls and Domain already references Analytics (see the `RangedVariable` reuse note at `Domain/SourceSpec.fs:100-103`), consider hosting one set of n/k series builders where both hosts can reach them; otherwise leave as-is -- the cost is cosmetic.

Considered and cleared (no finding): `WaveLengthInterval` (step 7) does not duplicate `Analytics`' `Range<'T>`/`WaveLengthRange` -- those are *sampled* ranges (point count) for sweeps, the new type is a coverage interval, and the core engine cannot reference Analytics; `SampleStackEditor` vs `StackEditor.groupLayers`/`applyStackMsg` -- the spec records a sound reason they are distinct (the engine editor has no material ids); the QWOT entry in step 22 -- no existing helper computes `λ/(4n)` (`Templates.dbrCell` at `Templates.fs:103` hardcodes thicknesses), so there is nothing to reuse; steps 15/16 copying the `LibraryControls` shape is pattern-*conformance*, not divergence.

## Bottom line

This spec is unusually reuse-disciplined -- every verified citation is accurate, it deletes existing duplication (the `Propagation.fs` string-branching and glass fallback), and it repeatedly names the exact constructor to build through. The substantive gaps are F1 and F2, both cheap to fix at the spec level (one sentence added to step 25; one ownership line added to step 21/23) and both likely to produce real divergent duplicates if workers execute the steps as written; F3 is a deliberate-looking trade-off the judge should confirm was chosen, not stumbled into. My read: worth a spec-text revision pass before implementation starts, not a re-architecture -- the judge decides.
