# Step 0468 — impl plan

Spec 0050 S14.1, layer 1 of the Measurement Planner: what a set of looks recovers of one target, and the Snell cap.

## Approach

In `OpticalConstructor.Domain/MeasurementPlanner.fs`, after step 467's `MeasurementConfiguration.plannerLookWith`:

1. `UnseenReason` — five cases in the brief's order (`NoLookReason`, `AlongEveryPropagationReason`,
   `NormalIncidenceOnlyReason`, `NoRetardanceReason`, `FullMuellerNeededReason`) with `member this.text : string`
   giving the brief's five sentences. Derived, never stored: no `value` tag.
2. `TargetSight` = `ReachableTarget | UnreachableTarget of UnseenReason`.
3. `type CrystalParameters with`:
   - `targetSightOf (looks : PlannerLook list) (tensorComponent : TensorComponent) : TargetSight` — no look →
     `UnreachableTarget NoLookReason`; else the first of seen → reachable, `ReachMissesInLook` →
     `FullMuellerNeededReason`, `NoRetardanceInLook` → `NoRetardanceReason`, `NormalIncidenceInLook` →
     `NormalIncidenceOnlyReason` that some look answers, otherwise `AlongEveryPropagationReason`. The parameter is
     `tensorComponent` because `component` is F#-reserved (as at step 467).
   - `snellCap : Angle` — `internalAngle` (MuellerInverse) of the smallest principal index at
     `IncidenceAngle.maxValue`. `RefractionIndex` is a plain single-case union, so `List.min` compares it
     structurally without `.value`.

In `OpticalConstructor.Tests/PlannerReportTests.fs`, three facts named exactly as in the effects, over the
existing probes and looks, plus a normal y-cut look for the triclinic g12 case. A private `componentNamed`
helper finds a parameter's component by name; `sightsOf` is rewritten to call it (assertions unchanged).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs`

No project file changes (both files are already compiled).

## Risks

- ELEV: new names are unique (grep found none); no bool projection of a two-case union; the priority table is a
  local list of `LookSight * TargetSight` (two distinct types, ELEV011-clean).
- `Angle` needs `open Berreman.Geometry` in the Domain file.
