# Step 0162 — implementation log

## Progress

- Read the system prompts, the project prompt, the slice, manual 013 and the gate snapshot.
  Checkpoint 160 in `.checkpoints-json` is `constructor_unit_tests: 1136`.
- Before the change, `git grep` finds no `KitHistoryLimit` or `NonPositiveKitHistoryLimitErr`
  under `Berreman/`, nor in STL (read-only grep of `C:\GitHub\Softellect`). The new type, its
  case and its error case are therefore unique (ELEV014 / ELEV016).
- `WorkbenchSettingsError` is used outside `WorkbenchSettings.fs` only in
  `WorkbenchSettingsTests.fs`. Every match there has a wildcard `Error e` arm, so the new case
  breaks no match. The App's `AppConfig.fs` does not match on the error cases.
- Edited the Domain file, then the test file (details below).
- Line endings: `git diff --numstat` and `--ignore-cr-at-eol` give the same counts for both
  product files (14/1 and 15/0), and `file` reports no CRLF, so there is no CRLF churn.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WorkbenchSettings.fs`
  - `WorkbenchSettingsError` gains `NonPositiveKitHistoryLimitErr of value : int` as its last
    case.
  - The error type's doc no longer counts the settings. "All three thresholds are
    counts/caps" became "Each setting is a count or a cap".
  - `KitHistoryLimit = KitHistoryLimit of int` is added right after `ThicknessBucketCap` and
    written the same way: `member this.value`,
    `static member defaultValue : KitHistoryLimit = KitHistoryLimit 100`, and
    `static member tryCreate (v : int) : Result<KitHistoryLimit, WorkbenchSettingsError>`.
    `tryCreate` answers `Ok (KitHistoryLimit v)` for `v > 0`, otherwise
    `Error (NonPositiveKitHistoryLimitErr v)`.
  - The type's doc is two lines: the most kits the kit history keeps; over it the oldest
    unstarred kit is dropped and a starred one never is (001 §F, spec 0050 S5.1). Default 100.
  - The `WorkbenchSettings` record and `defaults` are unchanged (step 163 carries the limit).
    No other type, case or member was added.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/WorkbenchSettingsTests.fs`
  - One new `[<Fact>]`:
    `KitHistoryLimit accepts a positive count, defaults to 100 and refuses a non-positive count with the typed error carrying the value`.
    It is placed after the `ThicknessBucketCap` theories. It checks three things:
    - `tryCreate` of 1, 100 and 5000 answers each value;
    - `KitHistoryLimit.defaultValue.value` is 100;
    - `tryCreate` of 0 and -1 answers `NonPositiveKitHistoryLimitErr` carrying the value.
  - It is a Fact, not a Theory, so it adds exactly one case. No existing fact changed.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates. The gate engine runs build,
elevation, constructor-unit-tests and the thirteen s19 invariants after exit. The only
commands run were read-only: `git grep`, `git diff` and `file`.

Expected test effect: constructor-unit-tests rises by exactly one case and one method identity,
from checkpoint 160's 1,136 to 1,137.

Every slice requirement is covered:

- the error case;
- the error type's reworded doc;
- `KitHistoryLimit` with `value`, `defaultValue` and `tryCreate`;
- the type's doc;
- the one declared test.

## Artifacts

None. The step produced no run artifact.

## Gotchas

- The slice cites `WorkbenchSettings.fs:15-18` and `:73-81`. At HEAD `a1d8c6c` both ranges are
  approximate:
  - the error type's doc and declaration are `:14-18`, with cases to `:20`;
  - `ThicknessBucketCap` is `:72-83`, doc through `tryCreate` body.

  The edits were placed by content, not by line number.
- `KitHistoryLimit` is deliberately NOT a field of `WorkbenchSettings` yet. Step 163 adds the
  field, the `defaults` entry and the matching test record updates. Adding it here would break
  the record literals in `defaults equals the record constructed directly…` and
  `a settings record assembled…`, which are outside this step's test effects.
- `specs/0050/.manifest.state.json` was already dirty, and it is supervisor-owned; it was left
  alone. No git mutation was made.
