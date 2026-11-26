# 0335 — impl plan

The fit machinery gets its product home: `configurationOf` and `observeWith` MOVE from
`BerremanTests/InverseFitHarness.fs` into a new `OpticalConstructor.Optimization/MuellerFitting.fs` (spec 0050 S10.1,
001 §Q AT-2, .spec-md §4 / §7). After the step exactly one declaration of each exists.

1. `OpticalConstructor.Optimization.fsproj`: one `<Compile Include="MuellerFitting.fs" />` directly after
   `MuellerInverseSolver.fs`, no comment.
2. `MuellerFitting.fs`, `module MuellerFitting` in `namespace OpticalConstructor.Optimization`, a 1-3 line module doc:
   - `type MuellerFitError = | ObservationForwardErr of ForwardModelError` (one case only; 337/339/341/342 add theirs);
   - `configurationOf (cut) (thickness) (incidence : float<deg>) (azimuth : float<deg>) (observable) (waveLength)`,
     `incidenceAngle = Angle.degree incidence |> IncidenceAngle.create`, `azimuth = SampleAzimuth.degree azimuth`;
   - `observeWith forward truth configurations : Result<MuellerObservation list, MuellerFitError>`, the
     `forwardModels` fold (`Ok []`, `Result.bind`, cons, `List.rev`): the first forward `Error e` is
     `Error (ObservationForwardErr e)` and nothing after it is evaluated.
3. `InverseFitHarness.fs`: delete both functions; add `expectOk (result : Result<'Value, MuellerFitError>) : 'Value`
   (`failwith $"%A{e}"` on `Error e`), one-line doc.
4. Callers: the four `configuration` helpers pass `(incidenceDeg * 1.0<deg>) (azimuthDeg * 1.0<deg>)`; the four
   `observe` helpers become `observeWith forward <truth> configurations |> expectOk`. Open `MuellerFitting` where the
   compiler asks (`AbsorbingBiaxialSample.fs` also needs `Berreman.Constants` for `deg` — already open).

Risks: bit-identity — `x * 1.0<deg>` is exact and `Angle.degree` receives the same double; the forward calls run in the
same order (the fold evaluates left to right, as `List.map` did). ELEV: measured `float<deg>` on a free function is the
form the tree already accepts (`MuellerReconstruction.angleMod180`); `MuellerFitError`/`ObservationForwardErr`/
`configurationOf`/`observeWith` are new unique names; the error case ends in `Err`. `ObservationForwardErr e` wraps a
typed error, not a bare primitive.

Verification (development runs; the gate engine is the authority): build, elevation, constructor-unit-tests, unit-tests
over `BerremanTests.FeasibleBiaxialInverseTests`, the nineteen s33 invariants.
