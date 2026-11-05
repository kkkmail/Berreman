# Step 0014 — IMPLEMENT — impl plan

## Approach

Obligation `S12-SWEEP` (spec 0050, section S1.2): after step 13 made
`MaterialEntry.complexity` mandatory, three test-side texts still describe the old
option (`complexity = None` / `complexity = Some`). This step rewrites them so
`git grep -n -E "complexity = (None|Some)" -- Berreman/` finds nothing, the grep that
S1.3's invariant `s03-complexity-mandatory` runs after every step. No product code
changes and no assertion changes.

1. `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs:459` and `:480`: the two comments
   say Silicon is view-only because its complexity is not editable term data (the
   `MaterialComplexity.isEditable` predicate; Silicon's axis is `PublishedLawAxis`).
2. `OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs`:
   - `siliconEntry`'s doc (`:27`) says Silicon's dispersion is the published silicon law;
   - the fact `spec 0040 006: the gyration builder draws all six components for a coded
     preset with no stored class` is renamed `… when every component is requested`
     (`RENAME`, successor id from the step's effects); its comment drops the coded
     preset and names the full set; the assertions are byte-identical.
3. `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj:120-124`: the existing
   comment ends at "Save round-trips through MaterialProxy". No comment is added.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Risks

- The renamed fact's display name is its test id: the successor string must match the
  effects' `successor` exactly (case, the `0040 006` prefix, no trailing text).
- LF line endings must survive the edits.
- The ui-tests count is unchanged (a rename, not an add or delete).
