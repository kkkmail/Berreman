# Step 0285 — impl plan (IMPLEMENT, S9.1 roles, sessions and notes)

## Approach

Pure Domain data first, then the one wire converter, then the tests.

1. `OpticalConstructor.Domain/Experiments.fs`, directly after `ExperimentId`, in this order:
   - `ExperimentRole` — seven two-word cases `ScienceRun` … `PlannedRun`; `value : int` 1–7, `tryOfWire`,
     `text` and `all` in tag order (the `KitStar` shape: int tag, no `Unknown*` case).
   - `SessionName of string` — `.value`, `tryCreate` keeping the text as given, `None` for blank / white space.
   - `ExperimentNotes of string` — `.value` and `none = ExperimentNotes ""`.
   Each doc comment one to three lines citing spec 0050 S9.1. Nothing else.
2. `OpticalConstructor.Storage/CanonicalJson.fs` — one `ClosedSetTagConverter<ExperimentRole>` registered after the
   `KitStar` one and before the `JsonFSharpConverter`; the `options` doc names it. `SessionName` / `ExperimentNotes`
   stay on the single-case unwrap.
3. Tests:
   - `ExperimentProxyTests` gains the two facts the slice names (role tags/texts/all; session name / notes).
   - `CanonicalJsonTests.every closed set with an int tag …` adds `assertWrittenAsTag<ExperimentRole>` and the
     explicit `DarkRun` ↔ `3` check; `an unknown disk tag is refused` adds tag 8 refused as `JsonException` by the
     converter AND as `MalformedJsonErr` from the real caller seam `parseVersionedText` (no file type holds a role
     before step 288, so an anonymous record carries it).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`

## Risks

- ELEV016 (unique DU case names): the seven `*Run` cases, `SessionName`, `ExperimentNotes` checked — no namesake in
  Berreman or STL.
- Opening `Experiments` in `CanonicalJson.fs` brings its free functions into scope; none collides with a name the file
  uses.
- Count must rise by exactly 2 (two new facts; the two changed facts keep their identities).
