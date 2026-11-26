# Step 0395 — impl plan: suspect thresholds in a study's fit settings (S11.4)

## Approach

1. `OpticalConstructor.Domain/Studies.fs`
   - `StudyError` gains `InvalidSuspectThresholdErr of reason : string`, rendered
     `the suspect thresholds are not valid: <reason>`.
   - Before `StudyFitSettings`: `FitCorrelation of float` and `SigmaFraction of float`, each with `.value`, and the
     record `SuspectThresholds = { correlation : FitCorrelation; sigmaFraction : SigmaFraction }` with
     `static member initial` (0.95, 0.1) and `static member tryCreate (correlation : float) (sigmaFraction : float)`
     keeping a correlation in (0, 1] and a finite fraction above 0, refusing anything else (NaN included) with
     `InvalidSuspectThresholdErr` naming the value.
   - `StudyFitSettings` gains `suspect : SuspectThresholds` as its last field; `initial` holds
     `SuspectThresholds.initial`.
   - `Study.withSuspectThresholds` as an intrinsic member beside `withFitStarts`, as step 382 placed `withFitStarts`
     beside `withFitMode` under the same `type Study with member ...` wording.
2. `OpticalConstructor.Storage/schema/study.schema.json`: `settings` requires `suspect`, an object of exactly
   `correlation` (`exclusiveMinimum` 0, `maximum` 1) and `sigmaFraction` (`exclusiveMinimum` 0). The serializer already
   unwraps single-case unions, so both are written as numbers with no converter.
3. Tests: every `StudyFitSettings` record expression gains `suspect = SuspectThresholds.initial`; the new
   `StudyTests` fact; the widened round-trip and schema-refusal facts in `KitContentFileJsonTests`; the new case in
   `StudySessionTests`' render fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/study.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs` (see risk below)

## Risks

- The shared study fixture `StudyFileFixtures.qzLrStudyOver` spells out a `StudyFitSettings` record and lives in
  `KitLibraryTests.fs`, not in `KitStudyFileTests.fs` as the effects list assumes. A record expression missing the new
  field does not compile, so how_to (3) ("every record expression that spells out a `StudyFitSettings` in the tests")
  requires the one-line edit there. Recorded as a deviation.
- The record label `correlation` also names a field of `FitQuality`'s record in Optimization; that record is declared
  after the opens there, so it keeps precedence. The build confirms.
