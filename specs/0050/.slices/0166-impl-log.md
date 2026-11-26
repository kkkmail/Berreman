# Step 0166 — IMPLEMENT — impl-log

## Progress

- [x] Register the `KitStar` int-tag converter in `CanonicalJson.options`.
- [x] Extend the two `CanonicalJsonTests` facts over `KitStar`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`
  - `open OpticalConstructor.Domain.KitHistory` (step 164's module that declares `KitStar`).
  - `options` registers `ClosedSetTagConverter<KitStar>((fun star -> star.value), KitStar.tryOfWire)`.
    It comes right after step 28's four closed-set registrations and before the `JsonFSharpConverter`.
    The generic converter writes `value` and reads through `tryOfWire`. An unknown tag, or anything
    that is not an int, throws `JsonException`, which the callers report as `MalformedJsonErr`.
  - The `options` doc comment's list of int-tag closed sets now names `KitStar` (step 166).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`
  - `open OpticalConstructor.Domain.KitHistory`.
  - `every closed set with an int tag is written as its tag and read back` (change_expectation,
    S51-STAR-TAG):
    - `assertWrittenAsTag<KitStar>` covers every case.
    - Explicit literal checks: `StarredKit` is written as `1` and `UnstarredKit` as `2`, and each
      reads back equal. These do not rely on `.value`, so a wrong mapping cannot hide behind itself.
  - `an unknown disk tag is refused` (change_expectation, S51-STAR-TAG) also requires
    `ofJson<KitStar> "3"` to throw `JsonException`.

No test was added, removed or renamed.

## Decisions

- I reused step 28's generic `ClosedSetTagConverter` rather than writing a fourth hand-written
  converter. The slice asks for a converter "written as step 28's four are", and step 28's four
  closed sets are all registrations of that one generic class, so a new class would duplicate it.
- I opened `KitHistory` instead of fully qualifying the name, matching how the four registrations
  use the opened `LibraryEntryFiles`. None of the names the module brings in (`KitStar`,
  `KitHistoryEntry`, `KitHistory`, `KitFolderPresence`, `KitHistoryError`, `KitHistoryProxy` and their
  cases) is used in either file, so nothing is shadowed.
- Nothing else changed: no other union changed its wire form, and the options' other converters and
  F# options are untouched.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates: no `dotnet build`, no `dotnet test`, no
analyzer and no invariant. After exit the gate engine runs:

- build;
- elevation;
- constructor-unit-tests (all discovered);
- the thirteen s19 invariants.

The only commands I ran were read-only (reads, grep, `git diff`, `git diff --numstat` against
`--ignore-cr-at-eol`, `file`) plus the two scripted text edits.

Expected: constructor-unit-tests stays at checkpoint 164's 1,149. Both edits are change_expectation
on existing facts, so the count is unchanged, as the acceptance requires.

## Artifacts

None. The step needs no captured logs.

## Gotchas

- `specs/0050/.manifest.state.json` was already modified when the round started. It is the
  supervisor's file, and I left it alone.
- Line endings: both edited files are LF. `git diff --numstat` matches `--ignore-cr-at-eol`
  (4/2 and 9/0), so there is no CRLF churn.
