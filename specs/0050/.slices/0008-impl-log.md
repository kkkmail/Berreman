# Step 0008 — IMPLEMENT — impl log

## Progress

- [x] Recorded the G17 reference literals from the start tree's closures before any edit.
  - The build output was newer than every source. Only a doc-comment touch in `Dispersion.fs`
    postdates the Domain dll, and the Core dll is newer still.
  - Two sources were read: the presets' private lets by reflection, and the library
    complexities' `EpsAxisEvaluated` axes. They agree bit-for-bit at every wavelength.
  - The script also recorded Langasite's g11 / g33, so their rewrite could be checked.
- [x] Core: `PublishedIndexLaw` and `PublishedLawAxis` (structural `Equals`, hash tag 7)
- [x] Engine presets read the laws: `Silicon`'s eps, and `Langasite`'s eps, g11 and g33
- [x] Library complexities carry the laws. The re-statements and their `WaveLength` extension
  are deleted.
- [x] Matches: `parameters`, `facetKey`, `isEditable`
- [x] Facts: the Silicon, Langasite and wire-tag facts
- [x] Build, a bit-for-bit check of the changed presets against the start-tree values, and every
  local check

## Files modified

- `Berreman/Berreman/Dispersion.fs`
  - `PublishedIndexLaw` (`SiliconIndexLaw` / `LangasiteOrdinaryIndexLaw` /
    `LangasiteExtraordinaryIndexLaw`), with `value` (1, 2, 3), `tryOfWire` and `complexIndex`.
    It sits after `BrendelBormannLaw`, before the eps tree.
  - `EpsAxisDispersion` gains `PublishedLawAxis of PublishedIndexLaw`, in its cases,
    `complexIndex`, `Equals` (structural), `GetHashCode` (tag 7) and its doc comment
- `Berreman/OpticalProperties/Dispersive.fs`
  - `Langasite`
    - deleted `numberE`, `re`, `refrIndexLa3Ga5SiO14Ordinary` and
      `refrIndexLa3Ga5SiO14ExtraOrdinary`
    - `realIndexSum` (Re nₒ + Re nₑ of the laws), the factor g11 and g33 used to re-state
      inline
    - `epsLa3Ga5SiO14` reads the two laws
  - `Silicon`: deleted `nSi`, `xiSiFinal` and `refrIndexSi`; `epsSi` is
    `Eps.fromComplexRefractionIndex` of `SiliconIndexLaw.complexIndex`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  - `siliconComplexity` / `langasiteComplexity` carry `PublishedLawAxis`
  - the private `WaveLength` extension with the three re-statements is deleted
  - rewrote the coded-preset comment block, `codedPresetInterval`'s doc and both complexities'
    docs; the file no longer names `EpsAxisEvaluated`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`: `parameters`'
  `SumOfTerms` arm answers `[]` for the case; its doc and inline comment
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`: `facetKey` answers
  `"Evaluated (transcendental)"`; its doc
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`:
  `isEditable` treats the case as no term data (view-only); its doc
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`: `open OpticalProperties.Dispersive`, the
  three facts, and `verifyLawReferences`' doc names step 8

## Decisions

- **The closed forms are moved verbatim.** Each expression is the `Dispersive.fs` text, character
  for character. `complexIndex` computes `lambda` and `numberE = exp 1.0` once, at its top, and
  each arm pipes its own value into `ComplexRefractionIndex`. A single pipe after the match would
  bind to the last arm only.
- **The Silicon arm keeps the names `nSi` and `xiSiFinal`** as local values. `refrIndexSi`
  becomes the arm's result.
- **g11 / g33 share `realIndexSum`.** Both re-stated the same two expressions inline, so one
  class-private helper now reads the laws' real parts once. The arithmetic order is unchanged
  (`(lambda * P) * (Re nₒ + Re nₑ)`), and the bit-for-bit check confirms the g values.
- **`.value.Real` reads a law's real part.** `ComplexRefractionIndex` has no real-part member,
  and `MaterialProperties.fs` is outside this step's effects. ELEV004 is measured, not gated.
- **No `NumericCarrier` mark on `PublishedIndexLaw`.** It is a fieldless closed set and carries
  no numbers.
- **Hash tag 7** continues steps 5 and 7's 0–6. `EpsAxisEvaluated` stays: step 9 deletes it.
  Only test fixtures construct it now (`OutOfBandDiagnosticTests`, `OutOfBandBadgeTests`).

## Testing state

These are local observations, made to confirm the change. The arc-runner's gate engine runs and
records the gates.

- `build`: exit 0, 0 errors. The first full build showed only the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree. A citation was added to a `MaterialLibrary.fs`
  comment after the checks; the rebuild after it showed 0 warnings and 0 errors.
- Bit-for-bit check (`.artifacts/0008-reference-verify.txt`): at every grid wavelength, the
  changed presets' eps equals `Eps.fromComplexRefractionIndex` of the recorded indices exactly.
  Langasite's g11 and g33 equal their recorded G17 values exactly. 0 failures.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 29/29 succeeded (26 before, plus
  the three new facts). The laws equal their literals exactly.
- `constructor-unit-tests` (whole suite): 734/734 succeeded. That includes `the silicon library
  entry reproduces the engine Silicon preset bit-for-bit`, `the re-seeded silicon and langasite
  eps reproduce the engine presets` and LibraryFacetsTests, all unchanged.
- `ui-tests` (MaterialsWindowTests, FsxAcceptanceTests): 49/49 succeeded.
- `ui-smoke`: 208/208 succeeded.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`. Core changed, so the run
  covered core and its dependents.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) each find nothing.
- LF line endings: `git diff --numstat` equals `--ignore-cr-at-eol`, and `git ls-files --eol`
  shows `w/lf` for every touched file.

## Artifacts

- `.artifacts/0008-reference-literals/closures.fsx`: reads the start tree's closures (presets by
  reflection, library axes) and g11 / g33
- `.artifacts/0008-reference-literals.txt`: the G17 literals it printed
- `.artifacts/0008-reference-literals/verify.fsx` and `.artifacts/0008-reference-verify.txt`: the
  bit-for-bit check of the changed tree's presets
- `.artifacts/0008-local-build.log`, `0008-local-build-final.log`, `0008-local-unit-tests.log`,
  `0008-local-constructor-unit-tests.log`, `0008-local-ui-tests.log`, `0008-local-ui-smoke.log`,
  `0008-local-elevation.log`

## Gotchas

- **The doc comments on Langasite's index lets were swapped in the start tree.**
  `refrIndexLa3Ga5SiO14Ordinary` was documented as "extraordinary", and the other way round. The
  CODE was consistent: `epsLa3Ga5SiO14` builds (ordinary, extraordinary, ordinary), and so does
  the library tree. The laws keep the code's naming. The swapped comments went with the lets.
- **Stale comments outside this step's effects, left alone:**
  - `OpticalConstructor.Domain/OutOfBandDiagnostic.fs:21` still calls the Silicon / Langasite
    dispersion "a coded closure". The behaviour it describes (an unbounded band, never flagged)
    is unchanged.
  - The test comments at `OpticalConstructor.Tests/MaterialComplexityTests.fs:153` ("ladder's
    evaluated rung") and `LibraryFacetsTests.fs:489` describe the old lowering. The slice keeps
    those facts unchanged, so they were not touched.
- **`.value.Real` precedence.** `(LangasiteOrdinaryIndexLaw.complexIndex w).value.Real` reads
  the law's index, then its `Complex`, then the real part. Keep the parentheses.
