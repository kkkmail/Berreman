# 0048 · 007 — E044: the three lists, checked against the row notes and the branch diff

**Row E044** (`002-elevation-plan.md` §6): every `[<ElevationBoundary>]` and `[<NumericCarrier>]` derived from git,
`renames.tsv` and `stored-format-changes.md`, each checked against the row notes and the branch diff. No build and no
suite. This closes the ELEV loop; what follows it is in the last section.

**Derived from git, not from the disk.** The two probes read tracked files at two refs. `92c11f7` is the branch's merge
base with `master`, and `b6cfc33` is the last commit before this one (E043). Re-run them to prove the tables:

    python specs/0048/.manual/tools/list_elev_marks.py 92c11f7 b6cfc33
    python specs/0048/.manual/tools/diff_package_surface.py 92c11f7 b6cfc33

`list_elev_marks.py` is STL's `specs/0219/.manual/tools/List-RingMarks.py` (`0219 .manual/026`) adapted to this tree.
It traces each mark to the oldest commit that added its attribute line and takes the row from that commit's message.
It then checks that the row's note names the declaration. `diff_package_surface.py` lists every public declaration
of the four packed projects that is gone at HEAD. It matches each one against `renames.tsv` and against the
"Declarations deleted" section of the row notes.

## In short

- **Nothing to correct.** All three lists agree with the row notes and with the branch diff. No list, note or source
  file is edited by this row.
- **Marks: 0 at the base, 43 at HEAD**, all added on this branch: **3 `ElevationBoundary`** (2 `IoBoundary` and
  1 `StorageBoundary`) and **40 `NumericCarrier`**. Seven are on private declarations. None is in a test project.
  No mark was reworded or removed after it was added: each of the 43 attribute lines appears once in the range's
  diffs, as an addition.
- **`renames.tsv`: 45 rows** (E003 14, E004 24, E005 7), the counts each row's note states. Every old name is gone at
  HEAD, and every new name is declared at HEAD (`System.Math.PI` is the BCL's).
- **`stored-format-changes.md`: 3 lines** (E022 1, E027 2). E022 and E027 are the only rows whose notes report a
  stored-format change; the other 41 say none and why.

## Marks

**By project:** Analytics 7, Core (`Berreman/`) 6, Controls 1, Domain 19, Optimization 2, Storage 3 — every one a
`NumericCarrier`. Storage also has 1 `StorageBoundary`, and App and TestWindows.App have 1 `IoBoundary` each.

**By row:** E003 6, E004 3, E005 4, E009 1, E013 1, E014 1, E015 10, E017 1, E020 3, E021 3, E022 2, E024 4,
E026 1, E028 1, E041 1, E042 1. Every other row's note says "Marks added: none", and git agrees.

**Against the notes.** Every one of the 43 is named in the note of the row that added it. For 21 marks the note
quotes the reason verbatim. For the other 22 it summarizes the reason without contradicting it: E003's six, E004's
three, E005's four, and E015's nine coefficient records, which the note gives as one template ("the <its
coefficients> of one <model> formula, read by its evaluation kernel at every wavelength of a sweep (R3; as
Dispersion.DispersionTerm[, the term data it lowers to])"). Every row-level claim matches the tree. Examples: E011's
"ChartPoint keeps E009's mark", and E020's "LeastSquaresSolution.solution reuses Core's RealVector carrier" (no
new mark).

**No `InteropBoundary`.** No declaration carries one. E022's note gives the reason for Alglib: its entry points are
private members of `OptimizationRequest`, and a member may speak its type's primitives. `elevation-full` passes
with no known findings (E043), so the analyzers ask for no further mark.

The tables below are `list_elev_marks.py`'s output, unedited but for the heading level.

### marks by project, 92c11f7 -> b6cfc33

| project | attribute | at base | at head |
|---|---|---|---|
| Analytics | NumericCarrier | 0 | 7 |
| Berreman | NumericCarrier | 0 | 6 |
| OpticalConstructor.App | ElevationBoundary | 0 | 1 |
| OpticalConstructor.Controls | NumericCarrier | 0 | 1 |
| OpticalConstructor.Domain | NumericCarrier | 0 | 19 |
| OpticalConstructor.Optimization | NumericCarrier | 0 | 2 |
| OpticalConstructor.Storage | ElevationBoundary | 0 | 1 |
| OpticalConstructor.Storage | NumericCarrier | 0 | 3 |
| OpticalConstructor.TestWindows.App | ElevationBoundary | 0 | 1 |
| OpticalConstructor.Ui | NumericCarrier | 0 | 2 |

### ElevationBoundary, added: 3

| # | row | commit | file:line | declaration | kind | in the row's note | reason, verbatim |
|---|---|---|---|---|---|---|---|
| 1 | E024 | `6e3b144` | `OpticalConstructor/OpticalConstructor.Storage/Report.fs:37` | `MaterialEntryDto` (private) | StorageBoundary | yes | the library file's materialEntry shape: System.Text.Json binds these fields by name from the file, and dtoToEntry elevates each one at the read |
| 2 | E041 | `35f92d8` | `OpticalConstructor/OpticalConstructor.TestWindows.App/Program.fs:33` | `main` | IoBoundary | yes | the process entry point - argv in, exit code out, per the .NET contract |
| 3 | E042 | `145ab22` | `OpticalConstructor/OpticalConstructor.App/Program.fs:221` | `main` | IoBoundary | yes | the process entry point - argv in, exit code out, per the .NET contract |

### NumericCarrier, added: 40

| # | row | commit | file:line | declaration | kind | in the row's note | reason, verbatim |
|---|---|---|---|---|---|---|---|
| 1 | E003 | `cbf0fc9` | `Berreman/Dispersion.fs:205` | `DispersionTerm` | - | yes | the centre, the coefficients, the exponent and the multiplier of one term of a dispersion formula, evaluated at every wavelength of a sweep (R3; as STL's TaylorApproximation) |
| 2 | E003 | `cbf0fc9` | `Berreman/Dispersion.fs:248` | `ComplexDispersionTerm` | - | yes | the centre, the coefficients, the exponent and the multiplier of one complex term of a dispersion formula, evaluated at every wavelength of a sweep (R3; as STL's TaylorApproximation) |
| 3 | E003 | `cbf0fc9` | `Berreman/MathNetNumericsMath.fs:32` | `RealVector` | - | yes | a MathNet vector: its elements are the solver's numbers (R3) |
| 4 | E003 | `cbf0fc9` | `Berreman/MathNetNumericsMath.fs:49` | `ComplexVector` | - | yes | a MathNet vector: its elements are the solver's numbers (R3) |
| 5 | E003 | `cbf0fc9` | `Berreman/MathNetNumericsMath.fs:88` | `RealMatrix` | - | yes | a MathNet matrix: its elements are the solver's numbers (R3) |
| 6 | E003 | `cbf0fc9` | `Berreman/MathNetNumericsMath.fs:119` | `ComplexMatrix` | - | yes | a MathNet matrix: its elements are the solver's numbers (R3) |
| 7 | E004 | `be932e5` | `Analytics/Variables.fs:187` | `SweepPoint` | - | yes | an element of a sweep array (R3): the plotted x of every point the solver ran, read as a raw double by Plotly, the series adapter and the colorimetry integral |
| 8 | E004 | `be932e5` | `Analytics/Variables.fs:196` | `SurfacePoint` | - | yes | an element of a surface sweep array (R3): the plotted x and y of every grid point the solver ran, read as raw doubles by Plotly and the series adapter |
| 9 | E004 | `be932e5` | `Analytics/Variables.fs:206` | `DispersionPoint` | - | yes | an element of a dispersion spectrum array (R3): the plotted wavelength and the property component at every sweep point, read as raw doubles by Plotly and the series adapter |
| 10 | E005 | `fbdf3a1` | `Analytics/Colorimetry.fs:106` | `ColourMatchingSample` (private) | - | yes | a row of a tabulated spectrum (R3): the CIE 1931 colour-matching functions at one wavelength, interpolated at every sample of the XYZ integral |
| 11 | E005 | `fbdf3a1` | `Analytics/Colorimetry.fs:117` | `IlluminantSample` (private) | - | yes | a row of a tabulated spectrum (R3): the D65 and A relative spectral powers at one wavelength, interpolated at every sample of the XYZ integral |
| 12 | E005 | `fbdf3a1` | `Analytics/Colorimetry.fs:127` | `SpectralSample` (private) | - | yes | an element of a tabulated spectrum (R3): one (wavelength, value) point the interpolation reads in its per-sample loop |
| 13 | E005 | `fbdf3a1` | `Analytics/FieldProfile.fs:31` | `FieldDepthPoint` | - | yes | an element of the field depth profile (R3, a profile array): the depth and /E/² at every sub-step of the walk, read as raw numbers by the series adapter |
| 14 | E009 | `f99eca2` | `OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs:116` | `ChartPoint` | - | yes | an element of a chart series (R3, a sweep array): the plotted x and y of every point of the run, mapped to the screen in the inline chart's per-point loop |
| 15 | E013 | `bb6cc7c` | `OpticalConstructor/OpticalConstructor.Domain/Placement.fs:44` | `Vector3` | - | yes | the components of a table-frame 3-vector, the numbers every rotation, reflection, refraction and crossing of the ray geometry computes with (R3; the serializable twin of the engine's MathNet-backed RealVector3) |
| 16 | E014 | `4fc776e` | `OpticalConstructor/OpticalConstructor.Domain/TableView.fs:33` | `ScreenPoint` | - | yes | the components of a screen-frame 2-vector in logical pixels, the numbers every projection, hit test, drag and drawing of the scene computes with (R3; the Avalonia-free twin of Avalonia.Point, as Vector3 is the table frame's) |
| 17 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:45` | `SellmeierCoefficients` | - | yes | the B and C coefficient lists of one Sellmeier formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to) |
| 18 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:55` | `CauchyCoefficients` | - | yes | the A, B and C coefficients of one Cauchy formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to) |
| 19 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:68` | `LorentzCoefficients` | - | yes | the eps-infinity and the per-oscillator strength, resonance and damping lists of one Lorentz formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to) |
| 20 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:80` | `DrudeCoefficients` | - | yes | the eps-infinity, plasma and damping frequencies of one Drude formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to) |
| 21 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:92` | `TaucLorentzCoefficients` | - | yes | the five coefficients of one Tauc-Lorentz formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm) |
| 22 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:105` | `GaussianOscillatorCoefficients` | - | yes | the four coefficients of one Gaussian-oscillator formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm) |
| 23 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:125` | `ForouhiBloomerCoefficients` | - | yes | the five coefficients of one Forouhi-Bloomer formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm) |
| 24 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:146` | `BrendelBormannCoefficients` | - | yes | the intraband coefficients and the per-oscillator lists of one Brendel-Bormann formula, read by its evaluation kernel (the Faddeeva series) at every wavelength of a sweep (R3; as Dispersion.DispersionTerm) |
| 25 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:162` | `ConstantNKCoefficients` | - | yes | the n and k of one constant complex index, the numbers its evaluation returns at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to) |
| 26 | E015 | `f6ca962` | `OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs:445` | `FaddeevaSeries` (private) | - | yes | the scale and the coefficients of Weideman's rational series, the numbers the Faddeeva kernel computes with for every Brendel-Bormann oscillator at every wavelength of a sweep (R3) |
| 27 | E017 | `460eaff` | `OpticalConstructor/OpticalConstructor.Domain/Propagation.fs:384` | `CurvePoint` | - | yes | an element of a computed sweep curve (R3, a sweep array): the swept x and the read y of every point a sweep solved or a run exported |
| 28 | E020 | `3afed6d` | `OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs:63` | `DesignRow` | - | yes | a row of a least-squares design matrix (R3): the coefficients one measurement imposes, stacked whole into the MathNet SVD solve |
| 29 | E020 | `3afed6d` | `OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs:186` | `LeastSquaresSystem` | - | yes | the right-hand side of a least-squares solve (R3): the measured intensities the MathNet SVD consumes whole, beside the design rows |
| 30 | E020 | `3afed6d` | `OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs:678` | `AirTrace` | - | yes | a measured series (R3): the dial angles and normalized intensities of an AIR calibration trace, mapped whole into a Stage-1 cosine-fit design |
| 31 | E021 | `3190c43` | `OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs:500` | `ResidualVector` | - | yes | the residual vector of a nonlinear least-squares fit (R3): built whole on every residual evaluation and handed whole to the Alglib Levenberg-Marquardt solve |
| 32 | E021 | `3190c43` | `OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs:801` | `ScaledVector` | - | yes | the optimizer's dimensionless parameter vector (R3): handed whole to the Alglib Levenberg-Marquardt solve as its start point and bounds, and back from it on every residual evaluation |
| 33 | E021 | `3190c43` | `OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs:951` | `ResidualJacobian` | - | yes | the residual Jacobian of a fit (R3): one row per residual entry and one column per fitted parameter, as FitQuality's finite differences produce it and the MathNet SVD and inverse consume it |
| 34 | E022 | `a64f096` | `OpticalConstructor/OpticalConstructor.Optimization/FitQuality.fs:56` | `ParameterMatrix` | - | yes | an n x n matrix over a fit's parameters (R3): the covariance reducedChi2 * (JtJ)^-1 the MathNet inverse yields, and the correlation derived from it whole |
| 35 | E022 | `a64f096` | `OpticalConstructor/OpticalConstructor.Optimization/OptimizationInterface.fs:20` | `DesignVector` | - | yes | the optimizer's parameter vector (R3): handed whole to Alglib as its start point, bounds and solution, and read on every residual evaluation |
| 36 | E024 | `6e3b144` | `OpticalConstructor/OpticalConstructor.Storage/Export.fs:50` | `ExportTable` | - | yes | the computed sweep or run results an export writes whole, one row of numbers per sample (R3: sweep and spectrum arrays) |
| 37 | E024 | `6e3b144` | `OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs:84` | `NkTableRow` (private) | - | yes | one row of an imported n,k table, a measured data series (R3): the interpolation closure reads every row at every wavelength of a sweep |
| 38 | E024 | `6e3b144` | `OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs:263` | `FormulaCoefficients` (private) | - | yes | the coefficient block of one refractiveindex.info formula page, read by position as the formula is lowered to term data (R3; as DispersionModels.SellmeierCoefficients) |
| 39 | E026 | `cbba6ac` | `OpticalConstructor/OpticalConstructor.Ui/Charts/SeriesData.fs:28` | `Surface` | - | yes | a surface sweep grid (R3): the Z value at every grid point the solver ran and the plotted x and y axis values, handed whole to the surface renderer |
| 40 | E028 | `8e3d2d0` | `OpticalConstructor/OpticalConstructor.Ui/SystemView3D.fs:41` | `Vec3` | - | yes | the components of a viewport 3-vector, the numbers the 3-D system view lays out and draws with (R3; the viewport twin of the table-frame Vector3, as ScreenPoint is of the 2-D screen) |

### added in the test projects: 0


### at base, gone at head: 0


## `renames.tsv`

**Scope.** Four projects are packed (`GeneratePackageOnBuild`): Core, OpticalModel, OpticalProperties and Analytics.
These are the rows' `packages` (002 §4). Between the base and HEAD, only E003, E004 and E005 touched them; no other
commit on the branch changes a file under those four folders.

**Each row of the list.** All 45 old names are gone at HEAD under their old owner. Each new name is declared at HEAD,
as the member, extension, static extension or union case the row describes. The one exception is `System.Math.PI`,
which is the BCL's.

**Each public name the packages lost.** The surface diff finds 69 public declarations that are gone at HEAD:

- 45 are the 45 rows of `renames.tsv`.
- The other 24 are deleted declarations, each named in its row's "Declarations deleted": 19 by E003 and 4 by E004,
  plus `FourierTransform.pi`. That last one gets a false match against the `MathNetNumericsMath.pi` row because the
  probe matches by short name; E003's note lists it as deleted ("FourierTransform pi, tau").
- None is unaccounted for.

The probe is textual. It does not see operators or the overloads of a name that survives, so E003's deleted scalar
`(*)` / `(/)` operators and `Item(Index, Index)` are covered by its note alone.

**Cross-check by identifier.** 39 identifiers occur in the four packages' sources at the base and nowhere in them at
HEAD:

- 31 are in `renames.tsv` or in a row note.
- 7 are locals or parameters: `k1`, `k2`, `sComp`, `yn`, `yo`, `zn` and `errMessage`.
- 1 is `defaultArg`, an FSharp.Core function that the code no longer calls.

So no public name left the packages without an entry.

**Rename or deletion — the line the list draws.** A public name that callers now reach under another name is a row.
A name with no caller outside its package, or whose callers now inline an expression, is a deletion recorded in the
note. `MathNetNumericsMath.pi` is a row because callers across the solution moved to `System.Math.PI`.
`FourierTransform.pi` had no caller outside its module, and `FourierTransform.tau` had one, in BerremanTests'
FFT test, which now inlines `2. * Math.PI`. So both are deletions. E003 applied that line consistently.

The table below is `diff_package_surface.py`'s output, unedited but for the heading level.

### public declarations of Berreman/Berreman, Berreman/OpticalModel, Berreman/OpticalProperties, Berreman/Analytics: 1173 at 92c11f7, 1276 at b6cfc33

### gone at b6cfc33 under their file and owner: 69

| file | owner | kind | name | still declared elsewhere | renames.tsv | in a note's Declarations deleted |
|---|---|---|---|---|---|---|
| `Analytics/AnalysisFunctions.fs` | `AnalysisFunctions` | let | `absorptance` | no | E005: Berreman.Solvers.Solution.absorptance (extension, Analytics.AnalysisFunctions) | - |
| `Analytics/AnalysisFunctions.fs` | `AnalysisFunctions` | let | `degreeOfPolarization` | no | E005: Berreman.Fields.StokesVector.degreeOfPolarization (extension, Analytics.AnalysisFunctions) | - |
| `Analytics/AnalysisFunctions.fs` | `AnalysisFunctions` | let | `ncs` | no | E005: Berreman.Solvers.Solution.ncs (extension, Analytics.AnalysisFunctions) | - |
| `Analytics/AnalysisFunctions.fs` | `AnalysisFunctions` | let | `psiDelta` | no | E005: Berreman.Solvers.Solution.psiDelta (extension, Analytics.AnalysisFunctions) | E005 |
| `Analytics/AnalysisFunctions.fs` | `AnalysisFunctions` | let | `stokesSystem` | no | E005: Berreman.Solvers.Solution.stokesSystem (extension, Analytics.AnalysisFunctions) | - |
| `Analytics/Colorimetry.fs` | `Colorimetry` | let | `xyzToLab` | no | E005: Analytics.Colorimetry.CieXyz.toLab (member) | - |
| `Analytics/Colorimetry.fs` | `Colorimetry` | let | `xyzToSrgb` | no | E005: Analytics.Colorimetry.CieXyz.toSrgb (member) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `ellipticity` | no | E004: Analytics.Variables.NumberOfPoints.ellipticity (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `ellipticityRange` | no | E004: Analytics.Variables.NumberOfPoints.ellipticityRange (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngle` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngle (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngle40` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngle40 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngle60` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngle60 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngle80` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngle80 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngleRange` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngleRange (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngleRange40` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngleRange40 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngleRange60` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngleRange60 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `incidenceAngleRange80` | no | E004: Analytics.Variables.NumberOfPoints.incidenceAngleRange80 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `polarization` | no | E004: Analytics.Variables.NumberOfPoints.polarization (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `polarizationRange` | no | E004: Analytics.Variables.NumberOfPoints.polarizationRange (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength05to20` | no | E004: Analytics.Variables.NumberOfPoints.wavelength05to20 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength05to20Range` | no | E004: Analytics.Variables.NumberOfPoints.wavelength05to20Range (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength200to800` | no | E004: Analytics.Variables.NumberOfPoints.wavelength200to800 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength200to800Range` | no | E004: Analytics.Variables.NumberOfPoints.wavelength200to800Range (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength250to600` | no | E004: Analytics.Variables.NumberOfPoints.wavelength250to600 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength250to600Range` | no | E004: Analytics.Variables.NumberOfPoints.wavelength250to600Range (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength500to700` | no | E004: Analytics.Variables.NumberOfPoints.wavelength500to700 (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardLightVariables.fs` | `StandardLightVariables` | let | `wavelength500to700Range` | no | E004: Analytics.Variables.NumberOfPoints.wavelength500to700Range (extension property, Analytics.StandardLightVariables) | - |
| `Analytics/StandardSystems.fs` | `StandardSystems` | let | `langasiteFilmOnSiliconSystem` | no | E004: Berreman.Dispersion.OpticalSystemWithDisp.langasiteFilmOnSiliconSystem (static extension, Analytics.StandardSystems) | - |
| `Analytics/StandardSystems.fs` | `StandardSystems` | let | `langasiteSubstrateOnSiliconSystem` | no | E004: Berreman.Dispersion.OpticalSystemWithDisp.langasiteSubstrateOnSiliconSystem (static extension, Analytics.StandardSystems) | - |
| `Analytics/StandardSystems.fs` | `StandardSystems` | let | `transparentGlass` | no | E004: Analytics.Variables.FixedInfo.transparentGlass (static extension, Analytics.StandardSystems) | - |
| `Analytics/Variables.fs` | `Variables` | let | `lineBrake` | no | E004: Analytics.Variables.ChartDescription.lineBreak (static member) | - |
| `Analytics/Variables.fs` | `Variables` | let | `removeLightVariable` | no | - | E004 |
| `Analytics/Variables.fs` | `Variables` | let | `waveLengthPlotMaxValue` | no | - | E004 |
| `Analytics/Variables.fs` | `Variables` | let | `waveLengthPlotMinValue` | no | - | E004 |
| `Analytics/Variables.fs` | `Variables` | let | `waveLengthPlotPoints` | no | - | E004 |
| `Berreman/BerremanMatrix.fs` | `BerremanMatrix` | let | `normalize` | yes | E003: Berreman.BerremanMatrix.l2Normalize | - |
| `Berreman/FieldFunctions.fs` | `FieldFunctions` | let | `toValue` | no | - | E003 |
| `Berreman/FieldFunctions.fs` | `FieldFunctions.FunctionDescription` | field | `description` | yes | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform` | let | `pi` | yes | E003: System.Math.PI | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform` | let | `tau` | no | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform` | module | `Details` | no | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform.Details` | let | `ilog2` | no | E003: Berreman.FourierTransformPrimitives.FftPointCount.log2 | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform.Details` | let | `isPowerOf2` | no | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform.Details` | let | `loop` | no | - | E003, E005 |
| `Berreman/FourierTransform.fs` | `FourierTransform.Details` | let | `twiddle` | no | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransform.Details` | let | `twiddles` | no | - | E003 |
| `Berreman/FourierTransform.fs` | `FourierTransformPrimitives` | let | `maxSize` | no | E003: Berreman.FourierTransformPrimitives.FftPointCount.maxValue | - |
| `Berreman/FourierTransform.fs` | `SimpleFourierTransform` | let | `fft` | yes | E003: Berreman.SimpleFourierTransform.simpleFft | E003 |
| `Berreman/Geometry.fs` | `Geometry` | let | `complexIdentityMatrix` | no | - | E003 |
| `Berreman/Geometry.fs` | `Geometry` | let | `complexZeroMatrix` | no | - | E003 |
| `Berreman/Geometry.fs` | `Geometry` | let | `realIdentityMatrix` | no | - | E003 |
| `Berreman/Geometry.fs` | `Geometry` | let | `realZeroMatrix` | no | - | E003 |
| `Berreman/Geometry.fs` | `Geometry` | let | `xRotation` | no | E003: Berreman.Geometry.Angle.xRotation | - |
| `Berreman/Geometry.fs` | `Geometry` | let | `yRotation` | no | E003: Berreman.Geometry.Angle.yRotation | - |
| `Berreman/Geometry.fs` | `Geometry` | let | `zRotation` | no | E003: Berreman.Geometry.Angle.zRotation | - |
| `Berreman/Geometry.fs` | `Geometry.Angle` | member | `radian` | no | E003: Berreman.Geometry.Angle (the union case) | E003 |
| `Berreman/Geometry.fs` | `Geometry.RealBasis3` | member | `toX` | yes | - | E003 |
| `Berreman/Geometry.fs` | `Geometry.RealBasis3` | member | `toY` | yes | - | E003 |
| `Berreman/Geometry.fs` | `Geometry.RealBasis3` | member | `toZ` | yes | - | E003 |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `complexDiagonalMatrix` | no | - | E003 |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `cplx` | yes | E003: System.Numerics.Complex.ofReal (extension, Berreman.MathNetNumericsMath) | - |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `createComplex` | yes | E003: System.Numerics.Complex.create (extension, Berreman.MathNetNumericsMath) | - |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `degree` | yes | E003: Berreman.Geometry.Angle.radiansPerDegree | E003 |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `pi` | yes | E003: System.Math.PI | E003 |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `realDiagonalMatrix` | no | - | E003 |
| `Berreman/MathNetNumericsMath.fs` | `MathNetNumericsMath` | let | `toDegree` | no | E003: Berreman.Geometry.Angle.radiansToDegrees | E003 |
| `Berreman/MatrixExp.fs` | `MatrixExp.ComplexMatrix` | member | `identity` | yes | - | E003 |
| `OpticalModel/Primitives.fs` | `Primitives` | let | `x` | no | - | E003 |
| `OpticalProperties/Standard.fs` | `Standard.RefractionIndex` | member | `getTransparentGlass` | yes | E003: Berreman.MaterialProperties.RefractionIndex (the union case) | E003 |

## `stored-format-changes.md`

**Against the notes.** The list has three lines:

- E022: `FitQuality.FitReport`'s `.fit-history.binz` sidecar.
- E027: `UserEnvironment.PanelState.visible`, now `PanelVisibility`.
- E027: `FavoritePin.MaterialPin`, now a `MaterialId`.

These are the only stored-format changes any note reports. The other 41 rows say "none" and give the reason. Each
row that touched a persisted type states that its stored shape is byte-identical, and gives evidence:

- E013 and E014 used serialization probes, plus the schema-validating round trips.
- E016 and E024: the material-library DTO is unchanged.
- E023: the `.ocproj`, `groups.json` and the sidecars are written by the same serializers.
- E042: the four keys of `appsettings.json` are unchanged.

**Against the branch diff.**

- **Schemas.** The only stored-shape file the branch changes is
  `OpticalConstructor.Ui/optical-constructor-environment.schema.json`: `panelState.visible` goes from `boolean` to
  the `PanelShown` / `PanelHidden` enum, in `f332bb2` (E027). That is E027's first line. The project and groups
  schemas did not change, and neither did any test fixture.
- **Binary sidecars.** The only product type pickled into a `.binz` is `FitReport` (`FitReport.toBinz`, through
  `SynthesisFitPage.saveFitReport`), which is E022's line. `Sidecar.writeSidecar` has no product caller at the base
  or at HEAD.
- **Stores with no schema.** E016 records that samples, user sources, the Library trees and the category catalogue
  are in-memory and never persisted. E019 records the same for experiment collections.

## What follows

The queue is empty with this row: E001–E044 are `done`. The whole tree is ELEV-green outside ELEV004 (excluded in
the command, R7), with an empty known-findings list (E043). The three lists stand as committed. Per 002 §5 and §6,
the spec's final step is the operator's by-hand run of all of BerremanTests, about 30 minutes. That run is not part
of this loop and comes before the arc is powered up.
