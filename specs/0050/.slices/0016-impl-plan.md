# Step 0016 — IMPLEMENT — impl plan

## Approach

Step 15 named every seeded preset through `Library.PresetIds`. Its ids still carry their step-15
text, so this step switches the constructor suite to the members. No fact changes its meaning or
its result (.spec-md §3.3; oracle obligation `S13-CONSTRUCTOR-FIXTURES`).

Every edit runs as a script that asserts how many times each pattern occurs
(`.artifacts/0016-scripts/preset-ids.py`), so no occurrence is missed or doubled.

1. `ExperimentCollectionProxyTests`, `ExperimentCollectionRoundTripTests`, `ExperimentProxyTests`:
   every `BoundByEntryId (ElementValueId "src-600" | "pol-lp" | "det-intensity")` becomes
   `BoundByEntryId PresetIds.source600 / polarizerLp / detectorIntensity`. All three files already
   open `OpticalConstructor.Domain.Library`.
2. `LibraryProxyTests`: `tryGetEntry PresetIds.source600`, then
   `Assert.Equal(PresetIds.source600, s.id)`. "no-such-id" stays.
3. `PlacedSampleEmissionTests`:
   - `entry` and `bind` take an `ElementValueId`;
   - `mirrorId`, `thinFilmId` and `plateId` hold the sample's `entryId`;
   - `sourceId` is `PresetIds.source600`.
4. `PolarizerBehaviorTests`:
   - the expected protected ids are the `.value` of the five detector and polarizer members, plus
     the seeded sources' ids, sorted as they are today;
   - `polarizer` and `extractedLabels` take an `ElementValueId` and compare ids;
   - their callers pass the members.
5. `SceneProxyTests:32` and `SceneRoundTripTests:32/:38/:105`: `Some PresetIds.source600` and
   `Some PresetIds.polarizerLp`. Neither file opens `Library`, so they write
   `Library.PresetIds.<member>`. Their existing `Library.ElementId` already uses that form.
6. `SourceLibraryTests`:
   - `wavelengthOf` takes an `ElementValueId`, and its seven checks read the members;
   - the live-Library detector reads `PresetIds.detectorIntensity`, both in its record and in the
     lookup.

## Files

The nine files in the slice's authorized effects, and no others.

## Risks

- **Name resolution.** `PresetIds` must resolve in each file. Five files open `Library`. The two
  Scene files do not, and they get the qualified form.
- **The test count must not move.** No `[<Fact>]`, `[<Theory>]` or `[<Trait>]` is added or
  removed. The baseline is 736.
- **Failure messages.** `entry`'s `failwith` renders the id as `%s{id.value}`, so its text stays
  the same.
