# Step 0207 — impl plan (IMPLEMENT: the parameter axes the existing models publish)

## Approach

`ItemModels.fs` (compiled right after `ElementId.fs`) gains the three opens the brief names — `Berreman.Media`,
`OpticalConstructor.Domain.MaterialLibrary`, `OpticalConstructor.Domain.Library` — and, after `ItemParameterAxis`,
four optional type extensions (spec 0050 S7.1, 001 §I):

- `Thickness.parameterValueOpt : ParameterValue option` — a finite thickness in metres; `Infinity` is `None`. The
  one conversion every thickness axis goes through.
- `SourcePreset.parameterAxes` — `azimuth`, `ellipticity` (`AngleParameter`, 0), `degreeOfPolarization`
  (`FractionParameter`, 0), `intensityScale` (`ScaleParameter`, 1): unpolarized light at unit scale (001 §N).
  λ and the tunable limits stay the preset's own fields.
- `PolarizerPreset.parameterAxes` — `[]` (an ideal LP / CP or a constant-Mueller compound has no parameter).
- `Sample.parameterAxes` — one `ThicknessParameter` per finite layer in stack order: `film<k>.thickness`,
  `film<k>.layer<j>.thickness` (a `Repeated` cell layer, shared by every period), then `substrate.thickness`.
  Materials publish nothing here (001 §I, S11.1).

No other type, member or option is added.

## Tests (`ItemModelsTests`, constructor-unit-tests +4)

- `a source preset publishes the light-source parameters, unpolarized at unit scale` — HeNe 633's axis list `=`
  the four axes.
- `an ideal polarizer preset publishes no parameter` — the LP preset's axis list is `[]`.
- `a sample publishes one thickness per finite layer` — glass plate 1 mm → `substrate.thickness` at 1e-3, glass
  film 600 nm → `film1.thickness` at 6e-7 (names and kinds by `=`, values within 1e-15); a test-local 5 nm layer
  plus a `Repeated` 10 nm / 20 nm cell → `film1.thickness`, `film2.layer1.thickness`, `film2.layer2.thickness`.
  `Thickness.Infinity.parameterValueOpt` is `None`.
- `a noise budget publishes its three uncertainties at its own values` — `detectorNoiseAxes` over step 206's
  three test-local values gives exactly the three expected axes (`=`). Step 206's three values are hoisted to
  private module bindings so both facts share one transcription (reuse, never copy).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`

## Risks

- ELEV: names must stay unique (`parameterAxes` / `parameterValueOpt` are new on each receiver); the sample
  member keeps primitive-typed inner function parameters out (names are built as `ParameterName` at origin).
- Opening `Library` / `MaterialLibrary` in `ItemModels` must not shadow anything `ItemModels` declares (it declares
  its own types after the opens, so its names win inside the module).
- LF line endings in both files.
