# Step 0048 state of the world

## Where we are

Spec 0050 S1.7 now obtains Details-band material names from the scene's live
materials store. Attempt 02 changes the private bandDisplayName return type
to the existing EntryName to address the supervisor's measured ELEV010P.
The scoped implementation and declared regression fact are complete.
Build, tests, analyzers and invariant execution belong to the
arc-runner after this worker exits; no successful gate result is claimed.

## What's working

- Resolve each band name through the supplied materials proxy as EntryName.
- Retain GUID labels for missing materials and failed lookups.
- Pass the scene materials store through the Details-band projection.
- Replace the band-shape fixture's seed sample with the Built-in catalogue.
- Add the declared catalogue-name and empty-store label regression fact.
- Document curated colours by MaterialIds while preserving both colour maps.

## Tests

Added `the band view names each band's material through the materials store it
is given` to MainWorkbenchTests. It requires exactly three quarter-wave bands
and checks the prefixes Transparent glass (n = 1.52), Vacuum and Transparent
glass (n = 1.52) over fresh catalogue stores. Over an empty materials store it
requires the corresponding glass, vacuum and glass GUID prefixes.

The existing `sampleBandsState collapses a period group to x-N bands (the
Details-bay shape)` now reads the sample and materials from BuiltinCatalogue.
Its assertions are unchanged. No existing test was deleted or renamed.

No build, tests, analyzers or invariant gates were run, per Invariant 6 and
the IMPLEMENT worker protocol. The supervisor's roster is build, elevation,
all ui-smoke tests, ui-tests selected to MainWorkbenchTests,
TableAndElementRotationTests, LayerBandsControlsTests and MainSceneMsgTests,
and all seven s07 invariants. Source and diff review found no line-ending
churn in the edited source files; this is not a gate result.

Attempt 02 preserves both acceptance facts and their assertions. It addresses
the ELEV010P diagnosis supplied by the supervisor; verification of the revised
source remains pending with that gate engine.

The smoke count below is inherited from checkpoint 46. ui_tests has no recorded
baseline there and retains the established zero absence marker. These are not
measurements from this round; the supervisor owns baselines.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

MaterialId.bandDisplayName accepts MaterialProxy, returns the existing
MaterialLibrary.EntryName and calls tryGetMaterial once. Ok (Some entry)
returns entry.name directly; Ok None and Error return the material's GUID
as EntryName. Sample.bandsState renders its value when composing the caption,
accepts the same proxy, and detailsState passes
model.materials. The private standard-library binding is deleted. No cache or
second lookup is introduced. Band ordering, thicknesses, repeat counts and
colours retain their existing projections.

The private return type deliberately differs from the slice's requested
string annotation: measured ELEV010P rejects that mixed primitive/elevated
signature, and AGENTS.md gives the analyzers precedence over document wording.
The repair reuses the existing name type, follows CategoryId.categoryName's
pattern and changes only an authorized file. Public bandsState behavior and
acceptance remain unchanged; no authority outside the declared effects is
needed.

Only an in-process member signature changes; no serialized or durable shape
changes and no migration is introduced. Both production and test callers use
the explicit materials argument.

## Deferred

No implementation requirement is deferred. Gate execution and acceptance are
assigned to the supervisor by the worker protocol; commit readiness remains
unclaimed until that verification completes.

## Gotchas

- The lookup returns Result<Option<_>>: both absence and failure keep a label.
- The material-store extension and VersionsInUse imports are needed by the
  empty-store test fixture.
- The pre-existing specs/0050/.manifest.state.json and
  specs/0050/.expected-red-observed-json edits were left untouched.
- No version, gate, oracle, analyzer configuration, external repository or
  git state was changed by this worker.

## Changelog

- 2026-11-06: Routed Details-band names through the scene materials store, retained GUID fallbacks, updated catalogue fixtures and added the declared regression fact.
- 2026-11-06 (attempt 02): Reused EntryName for bandDisplayName's return value to address measured ELEV010P, preserving labels and acceptance tests.
