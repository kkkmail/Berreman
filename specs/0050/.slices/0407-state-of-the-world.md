# Step 0407 state of the world

## Where we are

Spec 0050 S11.5 now has the pure promotion values that follow step 0406's
calibration revisions. This slice changes only `StudyPublishing.fs` and its
existing test module. Library persistence is outside this slice's authorized
effects. Implementation is complete; supervisor verification is pending.
Attempt 02 repairs the missing `Placement` import identified by the supervisor's
build failure; verification of that repair remains pending.

## What's working

- Added item-to-library identity conversions and fit provenance naming the kit,
  study file and fit-run file.
- Added material promotion preserving entry metadata and reconstructing the
  fitted crystal from fitted values and the recorded parameter snapshot.
- Added black-box promotion as a user-managed custom-Mueller preset at reference
  orientation, refusing non-black-box models and scoped or unfitted values.
- Added ordered promotion-subject discovery, five authorized test facts and
  coverage of the five additional publish errors.
- Imported the existing placement identity type for the item's preset conversion.

## Tests

No build, test, analyzer, invariant or other gate was run by this worker.
`implement_worker.system-md` Invariant 6 assigns all checks and gate verdicts to
the supervisor after worker exit. No passing result, duration, deadline compliance
or verified readiness is claimed.

The attempt-02 task supplies the preceding supervisor build result: exit 1 in
30.60 s, within its 600 s allowance, with three FS0039 diagnostics at
`StudyPublishing.fs:144` for the unresolved `ElementValueId` type/constructor.
The type is defined in `Placement.fs`; adding
`open OpticalConstructor.Domain.Placement` repairs the missing import without
changing behavior or tests. This is a source repair, not a new passing build result.

The pending roster is `build` (600 s), `elevation` (600 s),
`constructor-unit-tests` (180 s), and all twenty `invariant-s40-*` gates (300 s
each), exactly as frozen in `0407.gates`. The supervisor must run its unchanged
whole-suite constructor selection.

The five new facts cover fitted material values and metadata, scoped/unopened/
unfitted refusal, fitted black-box matrices and identity, provenance, and ordered
subject discovery. They also exercise working-state edits after the fit, missing
snapshot components, nonfinite fitted values, foreign studies, session ownership
and wrong item/material owners. The pre-existing error-rendering fact now covers
all eleven cases; its earlier assertions remain.

The YAML below preserves the preceding handoff's measured baseline. Five added
facts imply an expected total of 1735; that is an expectation, not a new measured
baseline or checkpoint.

```yaml
gates:
  constructor_unit_tests: 1730
```

## Architecture

- Promotion is pure: no database, file write, clock, source resolution or new
  serialized shape. `PromotionSubject` is derived and has no wire tag.
- Each material component passes through `OpenedMaterial.withParameterState`;
  only entry physics changes. The caller supplies the entry resolved for the
  opened material version.
- Black boxes bind the supplied model form through
  `BoundOpticalElement.ofOpticalForm`, using `ParameterValues.ofParameters`, then
  evaluate through `muellerAt` with `ElementOrientation.transmittingAt Angle.zero`
  and `SourcePreset.operatingWaveLengthOf None None`.
- Every fitted component must be owned by its material version or item. Unfitted
  components use the recorded fit snapshot, including known, calibrated or
  unknown states, rather than current working values.
- Fixtures reuse the shared fifteen-value absorbing crystal and shipped model
  entries through `BuiltinCatalogue.itemModel`; no model is manufactured.

## Deferred

No slice implementation is deferred. Gate execution remains the supervisor's
required next action under the IMPLEMENT worker protocol.

## Gotchas

- A missing non-fitted snapshot component is refused through the existing typed
  study/evaluation error wrapped by the new promotion error. The brief does not
  specify this malformed-input path; using current values would contradict its
  snapshot requirement. This choice is recorded in the implementation log.
- Subject discovery lists fitted candidates in study order followed by item
  order; promotion performs the ownership checks, as specified.
- The inherited `.manifest.state.json` modification belongs to the supervisor
  and was left untouched. No versions, gate descriptors, baselines, analyzer
  configuration, notebooks or git history were changed.
- Opening `Library` does not re-export its `Placement` imports. A declaration
  naming `ElementValueId` must import `Placement` directly or qualify the type.
- A structured `commit_ready: false` reports the absence of verified gate
  evidence, not an intentionally incomplete implementation.

## Changelog

- 2026-11-14: Implemented step 0407's pure fitted-material/black-box promotions,
  provenance, identities, subject discovery and five authorized regression facts.
- 2026-11-14 (attempt 02): Added the missing `Placement` import for
  `ItemId.toElementValueId` after the supervisor's FS0039 build diagnostic.
