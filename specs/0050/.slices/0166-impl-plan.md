# Step 0166 — IMPLEMENT — impl-plan

## Approach

The kit history stores a star as its int tag (AGENTS.md "a closed set's wire/disk tag is an integer",
`.spec-md` §4, step 28). `KitStar` (step 164, `OpticalConstructor.Domain/KitHistory.fs`) already carries
`value : int` (`StarredKit` 1, `UnstarredKit` 2) and `tryOfWire : int -> KitStar option`.

`CanonicalJson.options` gets one more `ClosedSetTagConverter` registration, for `KitStar`, written exactly as
step 28's four (`LibraryEntryKind`, `TabulatedInterpolation`, `PublishedIndexLaw`, `PublishedGyrationLaw`)
and registered with them, before the `JsonFSharpConverter`. The generic converter already writes `value` and
reads through `tryOfWire`, throwing `JsonException` on an unknown tag, which the callers map to
`MalformedJsonErr`. The `options` doc comment lists the closed sets, so `KitStar` joins that list. Nothing
else in the options changes and no other union changes its form.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`: open
  `OpticalConstructor.Domain.KitHistory`, register `ClosedSetTagConverter<KitStar>`, and add it to the doc
  comment's list.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`:
  - `every closed set with an int tag is written as its tag and read back` gains
    `assertWrittenAsTag<KitStar>`, plus explicit checks that `StarredKit` is `1` and `UnstarredKit` is `2`.
  - `an unknown disk tag is refused` gains `ofJson<KitStar> "3"` throwing `JsonException`.

Both are `change_expectation` edits: no test is added or renamed, so the count does not change.

## Risks

- Opening `KitHistory` in Storage brings the `KitHistory` record type into scope. CanonicalJson.fs does not
  use that name, so nothing is shadowed.
- ELEV: the registration follows the four existing lines, with lambdas over `.value` inside a member call
  (the same shape as today), so it adds no new finding shape.
