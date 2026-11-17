# Where we are

Step 0453 gives S1.1's tabulated n,k table optional per-sample one-sigma uncertainties of n and k (spec 0050 S13.2,
obligation S132-TABLE-SIGMA).

- Steps 443–452 built the online material sources, their pulls and the windows that host them.
- Here the table those pulls produce learns to carry the source's σ as data, serialised with it.
- A material pulled with σ keeps it in its entry file, in the database payload and in a kit's snapshot.
- No number of any table moves: `complexIndex` reads no uncertainty.

# What's working

- Added `TabulatedNkUncertainty`, a `NumericCarrier` record of `sigmaN` / `sigmaK` arrays, one value per sample.
- Gave `TabulatedNkTable` a last field, `uncertaintyOpt`. `tryCreate` keeps its signature and sets `None`.
- Added `TabulatedNkTable.withUncertainty`, the only way to set it:
  - a column whose length is not the table's answers `SampleCountMismatchErr`;
  - a NaN, infinite or negative value answers the new `InvalidUncertaintyErr`.
- `MaterialImport`'s tabulated import maps `InvalidUncertaintyErr` to `NoDataErr`, as it maps the other refusals.
- Added the two census facts:
  - the solver fact covers acceptance, unchanged indices and every refusal;
  - the entry-file fact round-trips a user material with and without σ.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority.

- **build:** exits 0 in 71 s, against a 600 s deadline, with 0 errors. The warnings are the exempt NU advisories and
  the vendored MathNet's SYSLIB0051, which predates this step.
- **unit-tests, filtered to `BerremanTests.EpsWithDispValueTests`:** `Passed!`, 26 of 26, in 2 s. That is the 25 facts
  at HEAD plus the step's one, `A tabulated table takes per-sample uncertainties of its own length and refuses any
  other`.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1802 of 1802, in 103 s against a
  180 s deadline. That is checkpoint 452's 1801 plus the step's one, `a tabulated material keeps its per-sample
  uncertainties through an entry file`.
- **ui-smoke, the whole suite as the gate selects it:** `Passed!`, 267 of 267, in 225 s against a 600 s deadline. The
  count is unchanged.
- **ui-tests, filtered to `MaterialEditorWindowTests` and `NkDispersionChartTests`:** `Passed!`, 44 of 44, in 3 s.
- **kit-acceptance, filtered to `OpticalConstructor.Ui.Tests.KitAcceptanceTests`:** `Passed!`, 14 succeeded and 1 skipped (AT-2's five-seed ensemble, xUnit v3 Explicit; the gate's skip policy is `allow`), in 1439 s against a 3600 s deadline. Both `… kit equals the committed kit file by file` facts are among the 14 that succeeded (only the ensemble skipped), and `data/` is untouched.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 226 s against a 600 s
  deadline. The first run, over the how_to's two-array signature, failed with ELEV010; see Architecture.
- **The twenty-one s44 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  berreman_unit_tests: 26
  constructor_unit_tests: 1802
  ui_smoke_tests: 267
  ui_tests: 44
  kit_acceptance_tests: 14
```

# Architecture

- **`withUncertainty` takes the carrier record:**
  `member this.withUncertainty (uncertainty : TabulatedNkUncertainty) : Result<TabulatedNkTable,
  TabulatedTableError>`.
  - The how_to spelled two `float array` parameters. Elevation reported that shape as ELEV010: bare primitives in a
    signature that names our own types. No boundary mark exempts it.
  - `tryCreate` escapes the rule only because it is one of the analyzers' `elevationPatternMembers`.
  - AGENTS.md: where a document disagrees with the analyzers, the analyzers win (spec 0048 `.manual/002` R6), and
    neither the rules nor the findings list may be edited. So the step's own `NumericCarrier` record is the
    parameter.
  - The contract symbol, the return type, both refusals and the facts' values are as briefed.
- **σ is data beside the columns.**
  - It is kept in the order given, never sorted or copied.
  - Nothing interpolates it. A reader takes a sample's σ from the columns.
  - `complexIndex` is untouched, so every number of every table, and of every material built on one, is unchanged.
- **No schema or committed file changes.**
  - The serializer (FSharp.SystemTextJson, `WithUnwrapOption`) writes the option as the record or `null` inside the
    payload, and the entry schema does not bind the physics (step 30).
  - No tracked catalogue entry or kit file holds a tabulated table, so no committed byte moves.
- R0001–R0005 are untouched: this step edits none of the files they govern.

# Deferred

Nothing.

# Gotchas

- **`LibraryFacetsTests.fs` gained one line although the effects' edits list omits it.**
  - Its fact `a tabulated axis lands on the tabulated n,k facet key` completely matches `TabulatedTableError`.
  - FS0025 is an error in every project (`--warnaserror+:25`), so the build fails until that match maps the new case.
  - how_to (2) asks exactly that of "every complete match", and `OpticalConstructor.Tests` is in `touches`.
  - No identity or assertion changed.
- **A member is not automatically exempt from ELEV010.** Only the names in the analyzers' `elevationPatternMembers`
  are (`value`, `tryCreate`, `create`, `ofWire`, `tryOfWire`, …). A new member that takes bare numbers beside our types
  needs an elevated parameter: a carrier or a single-case union.
- **The carrier can be built freely, but a table holds it only through `withUncertainty`.** A table that holds σ has
  therefore always been checked for length and finiteness.

# Changelog

- 2026-11-17, step 0453: added `TabulatedNkUncertainty`, `TabulatedNkTable.uncertaintyOpt` and `withUncertainty`
  (it takes the carrier, for ELEV010), and `TabulatedTableError.InvalidUncertaintyErr`, which the import maps to
  `NoDataErr`. Added one unit fact and one constructor fact: berreman_unit_tests over the class is 26, and
  constructor-unit-tests goes from 1801 to 1802.
