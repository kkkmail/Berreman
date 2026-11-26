# Step 0010 — IMPLEMENT — impl log

## Progress

- [x] Recorded the G17 reference literals of the start tree's `rhoLa3Ga5SiO14` before any edit
  - the build output was newer than every source
  - Langasite's private let was read by reflection and agrees bit-for-bit with the public
    `langasiteOpticalProperties.rhoWithDisp`
  - every entry other than ρ₁₁ / ρ₃₃, and every real part, is exactly zero
  - the values equal step 8's recorded g11 / g33
- [x] Core: `PublishedGyrationLaw` / `LangasiteGyrationLaw`, `RhoWithDispValue.PublishedLawRho`
- [x] Engine: the `toRhoWithDisp` arm; `Langasite` reads the law; the three lets and `realIndexSum` are deleted
- [x] Domain / Ui: the error case, `ofComplexity`, `isEditable`, the facets, the chart components, the caption
- [x] Docs: `langasiteComplexity`, the `PropagationTests.fs` comment, and the matching `MaterialComplexityTests.fs` comment
- [x] Facts: two in `RhoWithDispValueTests`, one each in `MaterialComplexityTests`,
  `LibraryFacetsTests`, `MaterialEditorWindowTests` and `NkDispersionChartTests`
- [x] Build and local checks

## Files modified

- `Berreman/Berreman/Dispersion.fs`
  - `PublishedGyrationLaw` (one case, `LangasiteGyrationLaw`), placed after `GyrotropicValue` and before `RhoWithDispValue`. Members:
    - `value` = 1
    - `tryOfWire` (`None` for any other tag)
    - `rhoAt : WaveLength -> Rho`. Its body is `realIndexSum`, `g11La3Ga5SiO14`, `g33La3Ga5SiO14` and
      `rhoLa3Ga5SiO14`, moved from `Dispersive.fs` with the code unchanged
  - `RhoWithDispValue` gains `PublishedLawRho of PublishedGyrationLaw`, and its doc names the case
- `Berreman/OpticalProperties/Active.fs`: `toRhoWithDisp` answers `RhoWithDisp law.rhoAt` for the case; its doc says so
- `Berreman/OpticalProperties/Dispersive.fs`
  - `Langasite`'s `rhoWithDisp` is `RhoWithDisp LangasiteGyrationLaw.rhoAt`
  - deleted `realIndexSum`, `g11La3Ga5SiO14`, `g33La3Ga5SiO14` and `rhoLa3Ga5SiO14`
  - removed the four opens nothing uses any more (`System.Numerics`, `Berreman.MathNetNumericsMath`,
    `Berreman.Geometry`, `Berreman.Constants`)
- `OpticalConstructor.Domain/MaterialComplexityEditor.fs`
  - `MaterialComplexityEditError` gains `PublishedGyrationNotEditableErr of reason : string`; the
    union's doc now covers `ofComplexity`
  - `isEditable` is `epsIsEditable && activeIsEditable`; the active aspect is editable unless it is
    `PublishedLawRho`
  - `ofComplexity`: the active seeding is a `Result` (`activeSeed`) and is combined with the eps
    seed through `Result.bind`. The published-law arm answers `PublishedGyrationNotEditableErr`, and
    its reason names the law ("Langasite"). The doc no longer says "seeding never fails"
- `OpticalConstructor.Domain/LibraryFacets.fs`: the gyration-class and handedness `extract` arms answer `[]` for
  `PublishedLawRho`. `appliesTo` is unchanged, so both facets still apply. Both docs say so
- `OpticalConstructor.Domain/MaterialLibrary.fs`: the `langasiteComplexity` doc says the constant uniaxial
  gyration stays, so no library number moves, and names `PublishedLawRho LangasiteGyrationLaw` as the
  engine preset's ρ
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: the `caption` arm for the new error
- `OpticalConstructor.Ui/NkDispersionChart.fs`: `gyrationComponentsOfComplexity` answers
  `allGyrationComponents` for the case; its doc says so
- `BerremanTests/RhoWithDispValueTests.fs`
  - `open OpticalProperties.Dispersive`
  - `The langasite gyration law reproduces the engine Langasite rho`
  - `The published gyration law round-trips its wire tag and refuses an unknown tag`
- `OpticalConstructor.Tests/MaterialComplexityTests.fs`
  - `a published gyration law keeps a complexity view-only and refuses to seed the editor`
  - the comment at `:155` no longer says Langasite's ρ has no value-tree form
- `OpticalConstructor.Tests/LibraryFacetsTests.fs`: `a published gyration law offers the activity facets but extracts no
  class or handedness`, over a local `materialEntryWith` entry, so no shared corpus changes
- `OpticalConstructor.Tests/PropagationTests.fs`: the langasite-film comment no longer says the ρ has no
  value-tree form
- `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`: `a published gyration law opens the editor view-only`
  - the entry is glass152 with the published-law complexity and that complexity's properties
  - `initMaterialEditor` gives `ViewOnlyMaterial _`
- `OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs`: `a published gyration law draws every gyration component`

## Decisions

- **The kernels are local functions of `rhoAt`, with their code unchanged.** The arm defines
  `realIndexSum`, `g11La3Ga5SiO14`, `g33La3Ga5SiO14` and `rhoLa3Ga5SiO14` with the text they had in
  `Dispersive.fs`, then answers `rhoLa3Ga5SiO14 w`. The expressions and their evaluation order are
  unchanged, and the fact's exact `Assert.Equal` confirms it at all five wavelengths.
  - `realIndexSum` moves with them: g11 and g33 read it, and the core cannot reach back into
    `OpticalProperties`.
  - Two doc comments only repeated the let names ("La3Ga5SiO14, g11." / "g33."), so they were
    deleted (AGENTS.md: delete a comment that repeats its identifier).
  - The `realIndexSum` comment became a `//` line comment on the local binding.
- **The law's name is rendered by a local match in `ofComplexity`.** The slice lists exactly three
  members for `PublishedGyrationLaw` (`value`, `tryOfWire` and `rhoAt`), so no `.text` member was
  added to the core type.
  - `%A` would breach ELEV002.
  - The match is exhaustive, so a second law forces a name.
- **`activeSeed` is evaluated before the eps seed is combined.** Both are pure, and the eps seed
  never fails, so the order cannot change a result. `Result.bind` pairs the two seeds, and the
  record build is unchanged.
- **The new facts build the constant isotropic n = 1.4 complexity locally.** This follows
  MaterialComplexityTests' AC-B7 fixture and LibraryFacetsTests' `constantIsoEps`. No shared fixture
  or corpus changes, so no existing count or facet sum moves.

## Testing state

These are local observations, made to confirm the change. The arc-runner's gate engine runs and
records the gates.

- `build`: exit 0, 0 errors.
  - The first full build showed only the two vendored MathNet SYSLIB0051 warnings that were
    already on the start tree (`.artifacts/0010-local-build.log`).
  - A comment was edited in `MaterialComplexityTests.fs` after the checks. The rebuild after it
    showed 0 warnings and 0 errors (`.artifacts/0010-local-build-final.log`).
- `unit-tests` (RhoWithDispValueTests and EpsWithDispValueTests): 38/38 succeeded.
  - The two new facts run by their exact ids, and both pass (2/2).
  - The law equals its G17 literals exactly.
- `constructor-unit-tests` (whole suite): 736/736 succeeded, which is step 9's 734 plus the two new facts.
  This was re-run after the final rebuild.
- `ui-tests` (MaterialEditorWindowTests, NkDispersionChartTests and FsxAcceptanceTests, with ui-smoke and
  kit-acceptance excluded): 57/57 succeeded. A separate run by id passed 4/4: the two LangasiteOnSilicon
  facts and the two new facts.
- `ui-smoke`: 208/208 succeeded.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`. Core changed, so the run
  covered core and its dependents.
- The four s02 invariants each find nothing (git grep exit 1): `elev-list-empty`, `no-vstest`,
  `no-kit-kit-json` and `no-eps-axis-evaluated`.
- LF line endings: `git diff --numstat` equals `--ignore-cr-at-eol`, and `git ls-files --eol`
  shows `w/lf` for all fourteen touched files.

## Artifacts

- `.artifacts/0010-reference-literals/closures.fsx`: reads the start tree's `rhoLa3Ga5SiO14` by reflection,
  cross-checks it against the public preset, and checks that the off-diagonal entries and real parts are zero
- `.artifacts/0010-reference-literals.txt`: the G17 literals it printed (0 failures)
- `.artifacts/0010-local-build.log`, `0010-local-build-final.log`, `0010-local-unit-tests.log`,
  `0010-local-constructor-unit-tests.log`, `0010-local-ui-tests.log`, `0010-local-ui-smoke.log`,
  `0010-local-elevation.log`

## Gotchas

- **The slice's `Dispersive.fs` line numbers predate step 8.** It cites `:40-49`, `:58-63` and `:70`.
  The lets were at `:23-49` and the `rhoWithDisp` field at `:56` on the start tree.
- **`ofComplexity` is not reached for a published law from the UI.** `initMaterialEditor` and
  `MaterialsWindowModel.editableSelection` gate on `isEditable` first. A published-law entry therefore
  opens view-only with the generic "coded engine closure" caption, and the new error's caption is
  shown only if a caller seeds without the gate.
- **The library Langasite still carries the constant ρ.** Only the engine preset reads the law. The
  FsxAcceptanceTests LangasiteOnSilicon facts store the engine preset itself, and they pass
  unchanged. Step 13 is the step that derives `properties` from the complexity.
- **Stale comment outside this step's effects, left alone.** `BerremanTests/LangasiteInverseTests.fs:104,109`
  cite `refrIndexLa3Ga5SiO14ExtraOrdinary` (deleted in step 8) and `g33La3Ga5SiO14`. The latter is now a
  local of `PublishedGyrationLaw.rhoAt`. The numbers those comments describe are unchanged.
- **`ui_tests` and `berreman_unit_tests` are filter-local counts.** This step's filters select
  different classes from step 9's. The 57 and 38 measured here are therefore not comparable with
  step 9's 66 and 30.
