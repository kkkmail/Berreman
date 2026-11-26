# Step 0467 — impl plan

## Approach

Add the Measurement Planner's look model (spec 0050 S14.1) as a new Domain module,
`MeasurementPlanner.fs`, appended after `PlannerSymmetry.fs` in the Domain compile list. It declares
exactly the six authorized contracts and nothing else:

- `DetectorReach` (`FullMuellerReach | EllipsometricReach | IntensityOnlyReach`);
- `LookTilt` (`NormalIncidenceLook | ObliqueIncidenceLook`) and `AxisAspect` (`TransverseAxis | LongitudinalAxis`);
- `PlannerLook` record `{ normalAxisOpt; incidence; observable; reach }` with `tilt`, `aspectOf` and `sightOf`;
- `MeasurementConfiguration.plannerLookWith` (type extension), reading a spec 0044 design as a look;
- `LookSight` (`SeenInLook | AlongPropagationInLook | NormalIncidenceInLook | NoRetardanceInLook | ReachMissesInLook`).

`sightOf` implements rules (a)–(e) of the brief by matching on the tensor component, on the reach and on
each axis's `aspectOf`. The transverse axes are gathered with `List.choose` over `AxisAspect` (never a
two-case-union-to-bool projection, ELEV012); the "two or more distinct index components" test is a list
pattern (`_ :: _ :: _`), so no bare int or bool enters a signature. `gyrationProjection` is not called
(the brief: it is the crystallographic reading, not the engine's). `internalAngle` is not needed here
(step 468 reuses it for the Snell cap).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (new, five facts)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>` line)

## Tests

The five facts of the brief, over the test project's literal `CrystalMaterialFixtures` (as step 465 did:
reuse, not re-transcription), each component looked up by its parameter name in `crystal.components`,
so the facts also exercise step 466's name-to-component mapping.

## Risks

- `component` is an F# reserved word: the `sightOf` parameter is named `tensorComponent`.
- ELEV016 / ELEV014: every new type and case name was grepped across the repo and STL `Sys`; none collide.
- The brief's normal-incidence test compares `incidence.value` with `IncidenceAngle.normal.value`;
  ELEV004 is excluded from the gate, and `IncidenceAngle.create` normalizes 0° to exactly 0.0.
