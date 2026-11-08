# Where we are

Step 0162 opens S5.1, the kit history over the per-user preferences. It adds the history's cap
as a workbench setting of its own: `KitHistoryLimit`, default 100, guarded by a typed
`NonPositiveKitHistoryLimitErr`. This follows 001 §F and `.spec-md` §8, where the only
configuration knobs are the 001 §G locations and `KitHistoryLimit`. The `WorkbenchSettings`
record does not carry the limit yet; step 163 adds it.

# What's working

- Add `KitHistoryLimit` (value, defaultValue 100, tryCreate) to the Domain `WorkbenchSettings` module
- Add `NonPositiveKitHistoryLimitErr of value : int` as the last `WorkbenchSettingsError` case
- Reword the error type's doc so it no longer counts the settings
- Add the `KitHistoryLimit accepts a positive count, defaults to 100 and refuses a non-positive count…` fact

# Tests

As Invariant 6 requires, this worker ran no gates: `gates_run: []`, `gate_results: {}`. The
gate engine runs build, elevation, constructor-unit-tests and the thirteen s19 invariants.

The step adds exactly one `[<Fact>]` and changes no existing fact. The constructor count should
rise by one, from checkpoint 160's 1,136 to 1,137.

```yaml
gates:
  constructor_unit_tests: 1137
```

# Architecture

`KitHistoryLimit` copies the pattern of the other workbench caps:

- a single-case DU over `int`;
- a `.value` reached only at the composition root;
- a `defaultValue`;
- a `tryCreate` that returns a typed `WorkbenchSettingsError` instead of throwing.

It lives in the Domain beside `ThicknessBucketCap`, so App configuration flows inward as an
elevated value.

# Deferred

Nothing in this step's scope. As the slice states, carrying the limit in the `WorkbenchSettings`
record is step 163's brief.

# Gotchas

- Adding `KitHistoryLimit` as a record field here would break the two record-literal facts in
  `WorkbenchSettingsTests.fs`. Step 163 owns that change, together with its test updates.
- No exhaustive match on `WorkbenchSettingsError` exists outside the tests. The test matches all
  carry an `Error e` wildcard arm, so the new case is non-breaking.
- `.manifest.state.json` is supervisor-owned and was left dirty as found.

# Changelog

- 2026-11-08: Step 0162 added `KitHistoryLimit` (default 100) and `NonPositiveKitHistoryLimitErr`
  to `WorkbenchSettings`, with one constructor fact.
