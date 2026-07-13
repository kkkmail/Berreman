# Step 035 — impl-plan (ADD_CONTRACT STORE_XDUO_0006 ExperimentDataProxy)

## Goal

Declare the measured-data **load seam** `ExperimentDataProxy` in
`OpticalConstructor.Domain` at the **declared** lifecycle: the IO boundary that
turns an experiment's elevated `DataFilePath` (step 025) into a parsed
`IntensitySeries` / `EllipsometricSeries`, or a typed `ExperimentDataError`
(step 034). Ship the interface plus a mock (canned series keyed by path) and a
mock-driven test that pins both fields' exact signatures. **No** real
disk-backed adapter — that is a later `IMPLEMENT_CONTRACT STORE_XDUO_0006`.

## Approach

Follow the arc's ADD_CONTRACT precedent (`Scene.SceneProxy` / `Scene.fs` +
`SceneProxyTests.fs`, and `ExperimentCollectionStore` step 028):

1. **New Domain file `ExperimentDataProxy.fs`**, module `ExperimentData`,
   declaring only:

   ```fsharp
   [<ReferenceEquality>]
   type ExperimentDataProxy =
       {
           tryLoadIntensity : DataFilePath -> Result<IntensitySeries, ExperimentDataError>
           tryLoadEllipsometric : DataFilePath -> Result<EllipsometricSeries, ExperimentDataError>
       }
   ```

   `DataFilePath` from `Experiments` (step 025); the series + error types from
   `MeasuredData` (step 034). The record is `[<ReferenceEquality>]` (function
   fields have no structural equality) so a host model that holds it keeps its
   Elmish-required equality — mirrors `SceneProxy` / `ExperimentProxy`.

2. **Register** `ExperimentDataProxy.fs` in the Domain `.fsproj` **after**
   `MeasuredData.fs` (it names both `Experiments` and `MeasuredData`; both must
   compile first — `MeasuredData.fs` is currently last).

3. **New Tests file `ExperimentDataProxyTests.fs`** (module
   `ExperimentDataProxyTests`): an inline mock keyed by `DataFilePath.value`
   (built over the step-34 parsers so the canned series are honest), and facts
   that (a) load a canned intensity + ellipsometric series through the exact
   signatures, (b) reject an unknown path with a typed `ExperimentDataError`
   (never a throw), and (c) confirm the proxy compares by reference. Bind each
   field to an explicitly-typed local first, so the compiler pins the exact
   signature the acceptance names.

4. **Register** `ExperimentDataProxyTests.fs` in the Tests `.fsproj` after
   `MeasuredDataTests.fs`.

## Files to modify

- `OpticalConstructor.Domain/ExperimentDataProxy.fs` — **new** (declared seam).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — register it.
- `OpticalConstructor.Tests/ExperimentDataProxyTests.fs` — **new** (mock + test).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register it.

## Risks

- **Compile order.** The seam names `Experiments.DataFilePath` and the
  `MeasuredData` series/error types; it MUST compile after `MeasuredData.fs`.
- **Miss semantics for the mock.** The declared `ExperimentDataError` channel
  (four cases, step 34) has no dedicated "path not found" case; a real store MAY
  grow the channel later. For the mock, an unknown path returns
  `MalformedDataFile` with a naming reason — I do NOT add a fifth error case
  (that would exceed the declared surface). Recorded in Gotchas.
- **Build gate word-`error` sensitivity + `--warnaserror+:25`.** The build gate
  fails on any `error` token in stdout, so the change must build perfectly
  clean. Verify locally.
- **LF endings.** New files must be LF (`.gitattributes`).

## Verification (advisory — the arc-runner's gate engine is the authority)

Build the solution (Release) and run the constructor test project locally to
confirm my own work compiles and the new facts pass; record counts in the SoW.
