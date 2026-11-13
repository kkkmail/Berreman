# Step 0382 — impl plan

## Approach

A study's fit gains its multi-start count as one of its fit settings (spec 0050 S11.3; 001 §L "fit settings").

1. **Domain (`Studies.fs`).**
   - Before `StudyFitSettings`: `FitStartCount of int` with `.value`, `tryCreate` (refusing a count below 1 with
     `InvalidFitStartCountErr`, naming the count) and `single` (1).
   - `StudyFitSettings` gains `starts : FitStartCount` as its last field; `initial` holds `FitStartCount.single`.
   - `Study.withFitStarts` is an intrinsic member written beside `withFitMode`.
   - `StudyError` gains `InvalidFitStartCountErr of reason : string`, rendered `the fit start count is not valid:
     <reason>`. It is appended last, so the existing error fact's indices stay as they are.
2. **Storage (`schema/study.schema.json`).** Under version 1, `settings` requires `starts`, an integer of at least 1.
   It stays `additionalProperties: false`. FSharp.SystemTextJson already unwraps single-case unions, so the
   serializer writes the count as its number; no converter is added.
3. **Tests.**
   - The two `StudyFitSettings` record expressions that spell the record out gain `starts = FitStartCount.single`:
     `StudyTests` and the `StudyFileFixtures` module in `KitLibraryTests.fs`.
   - The new `StudyTests` fact.
   - The widened round-trip and schema-refusal facts in `KitContentFileJsonTests`.
   - The new case in `StudySessionTests`' error-render fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/study.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`: one added field line. The file is not in
  the authorized edit list, but how_to (3) requires the change and the build depends on it (see the impl-log).

## Risks

- **`KitLibraryTests.fs` holds the shared `StudyFileFixtures`.** Its `StudyFitSettings` record expression must gain
  the field, or the test project does not compile. `KitStudyFileTests.fs`, which the edit list does name, spells out
  no `StudyFitSettings`.
- **ELEV.** `tryCreate (count : int)` is the type's own primitive on an elevation-pattern member, as
  `ResidualWeight.tryCreate` already is. Every new name (`FitStartCount`, `InvalidFitStartCountErr`, `withFitStarts`)
  is unique in the tree.
