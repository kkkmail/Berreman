# Step 0007 — CLOSE_RED — impl log

## Progress

- [x] Recorded the G17 reference literals from the start tree's `DispersionModel.evaluate` before
  anything moved. The step-6 build output (07:47) was newer than every source file, and production
  code was unchanged since step 5. The script and its output are in `.artifacts/`.
- [x] Core: `ForouhiBloomerLaw`, Weideman's Faddeeva series (private), `BrendelBormannLaw`, and the
  two axis cases (structural `Equals`, hash tags 5 and 6)
- [x] Domain: the intrinsic `toForouhiBloomerLaw` / `toBrendelBormannLaw`; `baseIndex`'s two arms
  answer the law, and their bodies and the Faddeeva block are deleted there; `toEpsAxis` lowers to
  the law cases, so the `EpsAxisEvaluated this.evaluate` line is gone; `parameters` returns `[]`;
  doc comments updated
- [x] `LibraryFacets.facetKey` maps both law cases to `"Evaluated (transcendental)"`.
  `MaterialComplexity.isEditable` treats both as no term data.
- [x] Facts: two new law facts, three changed expectations
- [x] Build, then every local check. A doc-comment touch in `Dispersion.fs` came after the checks;
  the rebuild after it showed 0 warnings and 0 errors.

## Files modified

- `Berreman/Berreman/Dispersion.fs`
  - `open MathNetNumericsMath`, right after `open System.Numerics` as in `Fields.fs`, so the
    moved bodies keep `Complex.create` verbatim (every later open still shadows it)
  - `ForouhiBloomerLaw` (`NumericCarrier`, with `complexIndex`)
  - `faddeevaTermCount`, `FaddeevaSeries`, `faddeevaSeries` and `faddeeva`, moved from the Domain,
    still private. Only the last doc line changed: it names `BrendelBormannLaw.complexIndex`
    instead of `baseIndex`.
  - `BrendelBormannLaw` (`NumericCarrier`, with `complexIndex`)
  - `EpsAxisDispersion` gains `ForouhiBloomerAxis` and `BrendelBormannAxis` in its cases,
    `complexIndex`, `Equals` and `GetHashCode`, and its doc comment
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
  - `ForouhiBloomerCoefficients.toForouhiBloomerLaw` and
    `BrendelBormannCoefficients.toBrendelBormannLaw`
  - the Faddeeva block is deleted
  - `baseIndex`'s two arms answer the law; their kernel bodies are deleted
  - `toEpsAxis` lowers both to their law cases
  - `parameters`' `SumOfTerms` arm answers `[]` for both
  - doc comments: the module header, `parameters`, `baseIndex`, `toEpsAxis`, `toEpsValue`,
    `toOpticalProperties`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`: `facetKey` and its
  doc comment
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`:
  `isEditable` and its doc comment
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`: the two law facts. `verifyLawReferences`'
  doc names steps 5 and 7.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`: the AC-B6
  lowering fact's match and comment (assertions unchanged)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs`: the step-015
  fact's match and comment, plus `Assert.False(c.isEditable)`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`: the
  pickable fact's match (per code: `ForouhiBloomerAxis` / `BrendelBormannAxis`) and comment

## Decisions

- **The kernels are moved, not rewritten.** Each law uses the self-identifier `c` (as step 5
  did), so each body is its `baseIndex` arm moved line for line over `x`. A line diff confirms it:
  every removed Domain line reappears in `Dispersion.fs`, except doc comments and the two final
  expressions, which gained `|> ComplexRefractionIndex`.
- **`open MathNetNumericsMath` in `Dispersion.fs`.** The moved bodies call `Complex.create`, an
  extension in `MathNetNumericsMath`. The open goes first, after `System.Numerics`, so every
  existing open still shadows it and no existing name changes meaning. The build confirms this.
- **The Faddeeva block sits between the two laws.** It is private to `Dispersion`, beside
  `BrendelBormannLaw`, its only caller.
- **`EpsAxisEvaluated` stays.** Step 9 deletes it, after step 8 turns the coded Silicon and
  Langasite presets (still closures) into data. Its doc now says no analytic model lowers to it.
- **Hash tags 5 and 6** continue step 5's 0–4.
- **The UI fact pairs code with case** (`"ForouhiBloomer", ForouhiBloomerAxis _`), so each pick
  must land on its own law. That is as strict as step 5's model/axis pairing in AC-B5.
- **The reference regimes.**
  - a-Si: Eg = 0.906 eV sits at 1368.5 nm. 300–900 nm lie above the gap; 2000 nm lies below it,
    with k exactly 0. This gives "both sides of the band gap".
  - Gold: interband absorption at 300 and 450 nm; the free-carrier metal beyond.

## Testing state

These are local observations, made to confirm the change. The arc-runner's gate engine runs and
records the gates.

- `build`: exit 0, 0 errors. The first full build showed only the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree. The rebuild after the final doc-comment touch:
  0 warnings, 0 errors.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 26/26 succeeded (24 before, plus
  the two new law facts). Both facts equal their literals exactly.
- `constructor-unit-tests` (whole suite): 734/734 succeeded. That includes the step-6 red
  `equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data`, the changed AC-B6
  and step-015 facts, and the unchanged AC-B6 reference facts.
- `ui-smoke`: 208/208 succeeded.
- `ui-tests` (MaterialEditorWindowTests): 29/29 succeeded.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`. Core changed, so the run
  covered core and its dependents.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) each find nothing.
- LF line endings: `git diff --numstat` equals `--ignore-cr-at-eol`, and `git ls-files --eol`
  shows `w/lf` for every touched file.

## Artifacts

- `.artifacts/0007-reference-literals/evaluate.fsx`: the script that read the start tree's
  `DispersionModel.evaluate`
- `.artifacts/0007-reference-literals.txt`: the G17 literals it printed
- `.artifacts/0007-local-build.log`, `0007-local-build-final.log`, `0007-local-unit-tests.log`,
  `0007-local-constructor-unit-tests.log`, `0007-local-ui-smoke.log`, `0007-local-ui-tests.log`,
  `0007-local-elevation.log`

## Gotchas

- The slice's line citations had drifted. `DispersionModelsTests.fs:403-430` / `:534-552` are now
  `:432-465` / `:570-588` (step 6 hoisted the builders), so the facts were found by name.
- The fact ids `AC-B6 ForouhiBloomer and BrendelBormann lower to the evaluated case and match
  evaluate` and `ForouhiBloomer and BrendelBormann are pickable and lower to the evaluated segment`
  still say "evaluated". The ids are the contract in the step's effects, so only the matches and
  comments changed.
- `ForouhiBloomerLaw` brings the labels `a`, `b` and `c` into `Berreman.Dispersion`.
  `BrendelBormannLaw` brings `resonance` / `broadening` as `float list`, beside
  `TaucLorentzLaw`'s `float` ones. Every construction site is annotated or constructor-directed,
  and the build is clean. Keep it that way.
