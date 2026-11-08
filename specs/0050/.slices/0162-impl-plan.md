# Step 0162 — implementation plan

The kit history's cap becomes a workbench setting of its own (001 §F, spec 0050 S5.1).
`OpticalConstructor.Domain/WorkbenchSettings.fs` gets `KitHistoryLimit`, written as
`ThicknessBucketCap` is. `WorkbenchSettingsError` gets its `NonPositiveKitHistoryLimitErr`
case.

## Approach

1. `WorkbenchSettingsError` gains `NonPositiveKitHistoryLimitErr of value : int` as its last
   case. Its doc stops counting the settings ("All three thresholds" becomes "Each setting is a
   count or a cap").
2. After `ThicknessBucketCap`, add `KitHistoryLimit = KitHistoryLimit of int` with
   `member this.value`, `static member defaultValue = KitHistoryLimit 100` and
   `static member tryCreate (v : int) : Result<KitHistoryLimit, WorkbenchSettingsError>`.
   The doc is at most three lines: the most kits the history keeps; over it the oldest unstarred
   kit is dropped and a starred one never is (001 §F, spec 0050 S5.1).
3. `WorkbenchSettings` record unchanged (step 163 carries the limit).
4. One new `[<Fact>]` in `OpticalConstructor.Tests/WorkbenchSettingsTests.fs`, with the
   slice's exact name. It checks: `tryCreate` of 1, 100 and 5000 answers each value;
   `defaultValue.value` is 100; 0 and -1 answer `NonPositiveKitHistoryLimitErr` carrying the
   value. It is a Fact, not a Theory, so the constructor count rises by exactly one
   (1,136 → 1,137).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WorkbenchSettings.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/WorkbenchSettingsTests.fs`

## Risks

- Name uniqueness (ELEV014 / ELEV016): `git grep` finds neither `KitHistoryLimit` nor
  `NonPositiveKitHistoryLimitErr` anywhere under `Berreman/` before the change.
- No exhaustive match on `WorkbenchSettingsError` exists outside the tests. The existing test
  matches carry a wildcard `Error e` arm, so the new case breaks no match (to be confirmed by
  grep).
- LF line endings must be kept.
