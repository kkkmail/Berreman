# Reuse critique -- 007.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (`git diff HEAD` additions plus the adjacent engine / storage / Analytics / test helpers they could reuse).
- Files inspected: ~16/200 — the diff files (`SourceSpec.fs`, `SourceCombination.fs`, `Project.fs`, `Sources/SourceEditorView.fs`, `Sources/PolarizationPicker.fs`, `SpectralImport.fs`, the `.fsproj`/test edits) plus `MaterialImport.fs`, `Analytics/Variables.fs`, `Constants.fs`, `Geometry.fs`, `MathNetNumericsMath.fs`, `Fields.fs`.
- Extensions: the task bounds named `.py,.md,.json`, but Berreman is a pure-F# repo and the whole diff is `.fs`; I walked `.fs` (plus the `.fsproj` edits) as the only defensible interpretation and record the mismatch here.

## Findings

### F1: `SpectralProfile.sample` re-derives the `WaveLengthRange` interpolation that `RangedVariable.value` already computes in canonical meters

- **Worker added:** the `WaveLengthRange r` arm of `SpectralProfile.sample` (`OpticalConstructor.Domain/SourceSpec.fs`, ~lines 200–213): `let s = r.startValue.value` / `let e = r.endValue.value` / `let n = max 1 r.numberOfPoints` / `[ for i in 0 .. n -> let m = s + (e - s) * (float i) / (float n) in WaveLength.Nm (m / nmToMeter) ]`.
- **Existing helper:** `RangedVariable.value : int -> float` at `Berreman/Analytics/Variables.fs:68`, whose `WaveLengthRange r` case (`:82–85`) is character-for-character the same formula — `s = r.startValue.value`, `e = r.endValue.value`, `s + (e - s) * (double i) / (double r.numberOfPoints)` — and returns precisely the canonical-meter magnitude `m`. `SourceSpec.fs` already `open`s `Analytics.Variables`, so `range.value i` is in scope.
- **Why it matters:** the worker correctly steered around `Analytics.getWaveLengthValue` (`Variables.fs:125`) — the SoW Gotchas record that it re-wraps the meter magnitude through `WaveLength.create`/`1.0<nm>` and mis-scales `.value`. But `RangedVariable.value` is the *un*-scaled sibling: it yields the bare meter float, which is exactly the `m` the worker then re-wraps as `WaveLength.Nm (m / nmToMeter)`. By hand-rolling the arithmetic the worker duplicated the engine's own range semantics; the same idiom now also lives in `MaterialImport.exportCsv:177–185`, so the interpolation exists in three places. If the engine ever changes endpoint handling or spacing, the source layer silently diverges. The slice text names the Part F sweep layer as the next range-sampling consumer, so a fourth copy is foreseeable.
- **Suggested action:** in the `WaveLengthRange r` arm, take `let m = range.value i` (or `(WaveLengthRange r).value i`) over `i in 0 .. n` and keep only the `n = max 1 …` zero-guard and the `WaveLength.Nm (m / nmToMeter)` re-wrap. Advisory — the judge weighs whether the `n=0` guard difference (the engine member divides by raw `numberOfPoints`) justifies retaining the local copy.

### F2: `SpectralImport` re-implements `MaterialImport`'s private CSV-parse skeleton (same Storage assembly)

- **Worker added (`OpticalConstructor.Storage/SpectralImport.fs`):** the private `inv` (`:25`), `tryFloat` (`:27–30`), `unitFromHeader` (`:34–38`), and the `parseSpectrumCsv` body (`:46–69`) — `try` / `CsvFile.Parse(csvText)` / `match csv.Headers with Some h when h.Length > 0 -> unitFromHeader h.[0]` / `csv.Rows |> Seq.choose (cols.Length >= 2 … match tryFloat cols.[0], tryFloat cols.[1] …)` / `toMeters unit …` / empty → an `Error` no-data case.
- **Existing helper:** `OpticalConstructor.Storage/MaterialImport.fs` carries the *same* private `inv` (`:34`), `tryFloat` (`:36–39`), `unitFromHeader` (`:46–53`), and the same `CsvFile.Parse` + header-unit + `Seq.choose` row loop in `importCsv` (`:144–165`). Both modules sit in the `OpticalConstructor.Storage` namespace/assembly. The worker's comment ("Mirrors `MaterialImport.unitFromHeader`", `SpectralImport.fs:33`) confirms the copy was deliberate.
- **Why it matters:** `inv`/`tryFloat` are byte-identical. `unitFromHeader` has already *drifted*: `MaterialImport` tests `"nm"` first and defaults `Micrometer` (`:50–53`); `SpectralImport` drops the explicit `"nm"` branch and defaults `Nanometer` (`:36–38`). The differing default is intentional (spectra are nm-conventional), but this is exactly the silent skew copied-not-shared helpers invite — a future fix to the detection ladder (quoted fields, BOM stripping, a culture tweak) must be applied twice or the two drift further.
- **Suggested action:** promote `inv`, `tryFloat`, and a parameterised `unitFromHeader (defaultUnit)` into one internal Storage helper (e.g. a `CsvParsing` module), optionally with a `parseTwoColumn` row reader, and have both importers consume it with the default unit passed as an argument. Leaving the duplication is defensible only if recorded as deliberate.

### F3: `SourceCombination.scaleStokes` hand-indexes a scalar–vector multiply the engine already provides

- **Worker added:** `SourceCombination.scaleStokes` (`OpticalConstructor.Domain/SourceCombination.fs:21–24`): `let (StokesVector v) = stokes in StokesVector.create [ weight * v.[0]; weight * v.[1]; weight * v.[2]; weight * v.[3] ]`.
- **Existing helper:** `RealVector.(*) (a : double, RealVector b)` at `MathNetNumericsMath.fs:41` — component-wise scalar × vector. `StokesVector` wraps `RealVector4` (`Fields.fs:580–581`), which wraps `RealVector` (`Geometry.fs:128–129`); the engine already defines scalar `(*)` for `RealVector2`/`RealVector3` (`Geometry.fs`).
- **Why it matters:** the four-element splat re-derives the engine's scalar-multiply and bakes in "a Stokes vector has four positional components," so a future shape change keeps the engine operator correct while this copy silently would not. It sits in the one reducer the slice's risk section calls load-bearing. The genuine constraint: unlike its siblings, `RealVector4` exposes only `+`/`-`/`Zero` (`Geometry.fs:136–140`) — there is no `RealVector4`-level scalar `(*)`, so the cleanest reuse routes through the inner `RealVector`.
- **Suggested action:** `let (StokesVector (RealVector4 inner)) = stokes in weight * inner |> RealVector4 |> StokesVector`. (Adding a scalar `(*)` to `RealVector4`/`StokesVector`, mirroring `RealVector2`/`RealVector3`, would be a consistent engine extension rather than a fork — but that is engine code outside this slice's scope, so the inner-vector reuse is the lighter move.) Advisory.

### F4: net-new `[<Measure>] type K` declared in `SourceSpec.fs` diverges from the engine's measure locus (low confidence)

- **Worker added:** `[<Measure>] type K` inside `module SourceSpec` (`SourceSpec.fs:33`).
- **Existing helper:** every other unit measure in the tree (`meter`, `mm`, `mkm`, `nm`) is declared in `Berreman.Constants` (`Constants.fs:5–18`).
- **Why it matters:** pattern divergence only — there is no Kelvin measure anywhere to reuse, so this is *not* duplication and the rubric forbids proposing a helper that does not exist. The only cost is locus: a reader scanning `Constants.fs` for "where measures live" will not find `K`. The countervailing force is real and recorded: binding constraint 2 forbids editing engine primitives, and `Constants.fs` is engine code, so relocating `K` there is heavier than this slice's scope wants.
- **Suggested action:** leave as-is; keep the in-file rationale (it already traces `K` to §E.5 and notes the engine carries no Kelvin). Flagged so the divergence is a recorded choice, not an accident. No reuse action mandated.

## Bottom line

F1 and F2 are substantive, citable near-miss duplications: F1 re-derives `RangedVariable.value`'s canonical-meter wavelength interpolation by hand (the engine member, not the mis-scaling `getWaveLengthValue`, is the right reuse target), and F2 maintains the `MaterialImport` CSV-parse skeleton a second time in the same Storage assembly with the unit default already drifting. F3 is a smaller real reuse miss in the load-bearing Stokes reducer; F4 is advisory pattern-only with a legitimate scope defence. My read: worth a cleanup pass, but none of these touch the correctness of the shipped, gate-green behaviour, so the judge may reasonably route them as follow-up rather than a re-spawn blocker.
