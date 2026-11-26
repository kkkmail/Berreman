# Step 0436: the forward model's allocation and the fit's bound

2026-11-16. Measured under R0004 (`.manual/035`): the concurrent residual gained nothing,
1.25 s per residual against 1.31 s serially, and AT-2's fit still missed 1800 s. The 44
concurrent rows each slowed from about 30 ms to 1.2 s with the test host near idle
(0.1 CPU-seconds per second) and its threads waiting on the collector. The garbage is the
solver's: `OpticalProperties.rhoT` (`MaterialProperties.fs`) builds a new conjugate
transpose on every access, and the generated terms of `BerremanMatrix.create` read
`o.rhoT.[i, j]` 92 times per matrix, `EmComponent.create` (`Fields.fs`) 10 times; every
process runs the default workstation collector. Measured AT-2 fit through the shell:
server GC alone 1397 s; `rhoT` read once alone over 1800 s; both 811 s (main solve 45 s),
and the step's fact passed with every quantity in its band. R0004's 900 s bound is too
close to that, and the operator doubles it.

## Binding ruling

At 0436, authorize editing `Berreman/Berreman/BerremanMatrix.fs` and `Berreman/Berreman/Fields.fs`
(hand edits of generated code, recorded here), and
`Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj` and
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`.

The rule: (1) `BerremanMatrix.create` and `EmComponent.create` read `o.rhoT` once into a
local and their terms read that local; every term and every number is unchanged.
(2) The app and the UI test host run the server garbage collector
(`ServerGarbageCollection`). (3) R0004 stands, except that AT-2's fit through
`runStudyFitThroughShell` must end within 1800 s, not 900 s, measured on the arc's
machine; R0004's conditional concurrent profile pass applies only above that bound.

Verify before commit: the build; elevation from the arc origin; and, filtered to their
classes, `OpticalConstructor.Ui.Tests.KitAcceptanceTests`,
`OpticalConstructor.Tests.FitResultsTests` and `OpticalConstructor.Tests.StudyEngineTests`,
recording AT-2's measured fit duration. This replaces R0004's verification roster; no
whole suite is run. Resume normally with executability profile 1 and the operator reply,
without `--accept-current-slice`, so normal review and the frozen roster publish
measured checkpoint 435. At 0437-0494 preserve the single `rhoT` read, the server
collector, the bound and R0001-R0004. The additional production authority is confined to
0436. No gate, floor, deadline, census, durable format, test effect or published-spec
amendment is authorized.
