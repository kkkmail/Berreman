# Reuse critique -- 004.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (the four new modules under
  `Berreman/OpticalConstructor/` plus their nearest engine helpers in
  `Berreman/Berreman/` and `Berreman/Analytics/`).
- Files inspected: ~14/200 (targeted walk, cap not reached): the four diff
  files, `Units.fs`, `Variables.fs`, `Charting.fs`, `MathNetNumericsMath.fs`,
  `MaterialProperties.fs`, `MaterialImportTests.fs`, plus the slice/SoW.
- Extensions: `.fs` substituted for the task's `.py` bound (F# repo), `.json`
  schema, `.md` read as context.

## Findings

Cycle-1's F1 (the isotropic `Mu.vacuum.dispersive`/`Rho.vacuum.dispersive`
builder spread over four sites) is resolved: `isotropicProperties` is now a
single Domain helper in `DispersionModels.fs` reused by `toOpticalProperties`,
`toAnisotropicOpticalProperties`, and `MaterialImport`. Not re-raised. The items
below are advisory consistency findings; none is a hard re-implementation of an
engine primitive.

### F1: `exportCsv` re-derives the n,k spectral sampling that `Variables`/`Charting` already compute from `OpticalPropertiesWithDisp`

- **Worker added:** `MaterialImport.exportCsv`
  (`OpticalConstructor.Storage/MaterialImport.fs:171-186`) hand-rolls the sample
  loop — steps `m = s + (e - s)*i/n` in meters, rebuilds a `WaveLength` per
  point, then recovers n as
  `Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0,0]`.
- **Existing helper:** `Analytics.Variables.calculateN11Re` / `calculateXi11Im`
  (`Analytics/Variables.fs:321-322`), built on
  `calculateOpticalProp EpsComp SquareRoot (One, One)` (`Variables.fs:303-318`),
  already sample an `OpticalPropertiesWithDisp` over a `Range<WaveLength>` and
  return exactly `Re[√ε₁₁]` / `Im[√ε₁₁]` — the same n and k `exportCsv`
  reconstructs by hand. The `SquareRoot` transform is the engine's own
  n-from-ε path.
- **Why it matters:** the √(ε₁₁) recovery and the per-point range walk are
  duplicated rather than delegated; if the engine ever changes how a component
  maps to a refraction index (branch/sign handling in the `SquareRoot`
  transform), the CSV exporter silently diverges from every dispersion plot.
- **Suggested action:** reuse `calculateN11Re`/`calculateXi11Im` for the n/k
  columns, OR leave as-is. The worker documented a real reason to avoid the
  obvious entry point: `getWaveLengthValue` (`Variables.fs:125-127`), which
  `calculateOpticalProp` calls internally, mis-scales a `Nm` range. That makes a
  literal swap unsafe today, so this is a judge call, not a defect — but the
  duplication should stay flagged as deliberate and tied to that engine quirk.

### F2: raw `System.Numerics.Complex(...)` constructors bypass the engine's `cplx`/`createComplex` seam

- **Worker added:** in `DispersionModels.baseIndex`
  (`OpticalConstructor.Domain/DispersionModels.fs`, the `Lorentz`/`Drude` arms)
  the code builds complex values directly: `Complex(s, 0.0)`,
  `Complex(r*r - x*x, -d*x)`, `Complex(c.epsInf, 0.0)`,
  `Complex(c.plasmaFrequency * c.plasmaFrequency, 0.0)`. The same function uses
  `createComplex` for the Sellmeier/Cauchy/ConstantNK arms.
- **Existing helper:** `Berreman.MathNetNumericsMath.cplx`
  (`MathNetNumericsMath.fs:14`, `cplx r = Complex(r, 0.0)`) and `createComplex`
  (`:15`) are the project's complex-construction seam — the module exists
  precisely to "abstract away differences in vector / matrix libraries"
  (`MathNetNumericsMath.fs:3`). `DispersionModels.fs` already `open`s the module
  and uses `createComplex` elsewhere, so both helpers are in scope.
- **Why it matters:** the raw-constructor calls defeat the abstraction
  `MathNetNumericsMath` exists to provide and leave the function internally
  inconsistent (two idioms for the same operation); a future library switch
  would miss these sites. The all-real cases (`createComplex x 0.0`,
  `Complex(x, 0.0)`) duplicate exactly what `cplx x` already names.
- **Suggested action:** route every zero-imaginary construction through `cplx`
  and every n+ik construction through `createComplex`, dropping the raw
  `Complex(...)` calls — a mechanical, behaviour-preserving cleanup (`createComplex r i`
  *is* `Complex(r, i)`, so the change is identity-preserving).

### F3: linear range-stepping `s + (e - s)*i/n` re-implemented in `axisTicks` and `exportCsv`

- **Worker added:** the identical interpolation expression
  `s + (e - s) * (float i) / (float n)` appears in `MaterialPreview.axisTicks`
  (`OpticalConstructor.Ui/MaterialPreview.fs:40-44`) and `exportCsv`
  (`MaterialImport.fs:177-182`).
- **Existing helper:** `RangedVariable.value`/`Range` encode exactly this
  stepping for a `Range<WaveLength>` (`Analytics/Variables.fs:82-85`, the
  `WaveLengthRange` arm; `plotPoints` at `:122`).
- **Why it matters:** two new copies of the engine's range-stepping formula
  drift independently from the canonical one — the same class as F1, with the
  same documented `getWaveLengthValue` Nm bug as the reason for not reusing.
- **Suggested action:** fold the stepping into one shared helper, or leave as-is
  and cross-reference the `Gotchas` note. Lowest priority; reasonably bundled
  with whatever fix the materials-wiring slice applies to the
  `getWaveLengthValue` quirk.

## Bottom line

The diff reuses the load-bearing engine seams correctly — `Eps.fromComplexRefractionIndex`,
the `getProperties`/`getEps` path, the `plotN11…` charting functions, the
`Units` conversion seam, and the now-shared `isotropicProperties` wrapper — and
cycle-1's only weighty finding is fixed. The three remaining items are
consistency nits: F2 is a clean mechanical fix (`cplx`/`createComplex`), while
F1 and F3 are near-misses the worker deliberately avoided because of the
documented `getWaveLengthValue` Nm mis-scaling bug. My read: not substantive
enough to compel a re-spawn on reuse grounds — F2 is the only one worth applying
inline if a route-back happens for another reason; the judge decides.
