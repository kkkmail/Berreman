# Where we are

Step 0163 continues S5.1, the kit history over the per-user preferences. Step 162 added the
`KitHistoryLimit` type. This step carries it in the `WorkbenchSettings` record (default 100) and
reads it from `appsettings.json` through a settings door, following 001 §F and §G. The history
itself, `KitHistory.bump`, takes the limit as a parameter in step 169.

# What's working

- Carry `kitHistoryLimit : KitHistoryLimit` in `WorkbenchSettings` (default 100)
- Add `ConfigKey.kitHistoryLimit` and read it in `AppConfig.loadWorkbenchSettings`, writing a missing or refused value back as 100
- Collapse the three count-door parse bodies into one private `ConfigValue.tryPositiveCount` and add the `KitHistoryLimit` door through it
- Make the shared count body generic in the refusal it discards, which clears ELEV010P
- Add `the default settings carry a kit history limit of 100` and the four-door AppConfig refusal fact, and extend the two record-literal facts

# Tests

As Invariant 6 requires, this worker ran no gates in either attempt: `gates_run: []`, `gate_results: {}`. The gate
engine runs build, elevation, constructor-unit-tests, ui-smoke, ui-tests and the thirteen s19
invariants.

Expected effects:

- constructor-unit-tests: +1 (one new Fact; two facts changed), from checkpoint 161's 1,137 to
  1,138.
- ui-tests over the selected classes: +1 (the new `AppConfigTests` fact).
- ui-smoke: unchanged at 224.

```yaml
gates:
  constructor_unit_tests: 1138
  ui_smoke_tests: 224
```

# Architecture

- The record stays all-elevated. `KitHistoryLimit` reaches the composition root as its own type
  and is never a raw int.
- Every count door now shares one body: `member private this.tryPositiveCount` on
  `AppSettings.ConfigValue`, declared in `WorkbenchSettingsDoors`. Each door passes its type's
  own int `tryCreate`.
  - Private to the module. ELEV010 still checks private signatures (ELEV010P), so its signature
    names none of our types: the intrinsic's refusal is the generic `'Refusal`, which the body
    discards. Attempt 01 typed it `WorkbenchSettingsError`, which made `int` / `string` sit beside
    an elevated type.
  - It replaces three copies instead of adding a fourth (AGENTS.md "Reuse, never copy").
- The key is read and saved with the other workbench keys through `readSetting`, so a refused
  value is logged before the default is written back.

# Deferred

Nothing in this step's scope. The limit's reader, `KitHistory.bump`, is step 169's brief per the
slice.

# Gotchas

- `<Type>.tryCreate` is overloaded: the intrinsic takes an `int`, the door takes a
  `ConfigValue`. When a door passes it to `tryPositiveCount`, the parameter type
  `int -> Result<_, 'Refusal>` picks the intrinsic, because the `int` domain disambiguates.
- A `member private` on an optional extension is private to the enclosing module, so other
  `type … with` blocks in the same module can call it. `KitFolderStore.readKitText` relies on the
  same rule.
- The census found only three `WorkbenchSettings` literals: `defaults` and two test facts.
  Several other records share the field names `quickPickThreshold`, `treeAutoBuildThreshold` and
  `thicknessBucketCap`, for example the workbench model and `LibraryWindowContext`. They are not
  this record.
- ELEV010 counts the curried parameters and the return, never an extension member's receiver.
  It counts a generic parameter as not elevated. A private helper is still checked (`P`
  surface), so naming an error DU in a helper's signature that also carries `int` / `string` is
  a finding.
- The shipped `appsettings.json` files do not list `KitHistoryLimit`. They are outside this
  step's edits, and the default is written back on first read.

# Changelog

- 2026-11-08: Step 0163 put `kitHistoryLimit` (default 100) in `WorkbenchSettings`, read it from
  `appsettings.json` through a `KitHistoryLimit` door, and merged the count doors' parse into one
  private member. It added two facts and extended two.
- 2026-11-08 (attempt 02): Elevation retry. `tryPositiveCount`'s refusal parameter is now generic
  (`'Refusal`) instead of `WorkbenchSettingsError`, which clears the gate's one ELEV010P finding.
  The doors, the texts and the tests are unchanged.
