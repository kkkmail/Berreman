# Where we are

Slice 0155 wires the launcher's locked-kit read-only offer after slice 0154's picked-folder constructor composition. The implementation and two authorized smoke facts are complete. The WIRE_UI worker protocol assigns executable verification to the supervisor after worker exit; no runtime pass result is claimed.

# What's working

- Added the holder diagnostic and accessible Open read-only / Cancel buttons beneath it in one horizontal row with a 12 px gap and shared positive/negative colours.
- Retried the same picked folder and constructor mode through the existing ReadOnlyKitAccess opener, without another picker call.
- Cleared the diagnostic and offer on success or Cancel, and retained the ordinary diagnostic without an offer for other errors.
- Authored both declared smoke facts covering real launcher input, row geometry, read-only Main/Inverse windows, refused scene saving, unchanged file bytes and retained holder locks.
- Extended the existing non-lock failure assertion and preserved WireUiFinalCompositionTests unchanged.

# Tests

No build, test, analyzer or invariant gate was executed by this worker. The supervisor runs build, elevation, constructor-unit-tests, ui-smoke, selected LauncherTests ui-tests and all eleven s17 invariants. Exactly two new ui-smoke identities were added; no existing test identity or assertion was removed. The acceptance test also checks that a scene-save request in the real opened window projects the existing read-only error into kitStatus. Both new tests close their separately held kit in finally; opened constructor windows also close in finally.

These counts are inherited from checkpoint 153, not measured or authored in this round. The two added facts are not counted as passed, and no whole-suite ui_tests baseline is invented.

```yaml
gates:
  constructor_unit_tests: 1149
  ui_smoke_tests: 219
```

# Architecture

LauncherWindow tracks the optional offer row beside its existing optional diagnostic and clears them together. The existing shared folder/mode/access opening function handles its read-only callback. Only KitLibraryKitErr (KitLockedErr holder) renders the offer; every other opening error retains the previous diagnostic wording. Buttons reuse surfaceButton for Name, AutomationId and accessible name, and MaterialsWindowView for their action colours.

KitOpening.openKitAt, the supplied-kit constructor initialization, storage read-only enforcement and Closed-handler ownership are reused without changes. There is no new durable shape, wire tag, format, migration or serialization path.

# Deferred

No implementation work was deferred. Executable verification is assigned to the supervisor by the act-only worker protocol; commit_ready remains false until that verification is available.

# Gotchas

- Retry callbacks retain the folder and mode of the failed attempt, so an Inverse retry opens Inverse and does not repick.
- A same-process second KitProxy still counts as another holder. The tests keep that original holder alive through read-only open, attempted save, constructor close and Cancel.
- Recursive file inventories and byte comparisons include .oc/lock; the original lock must survive the read-only window's close.
- Tests use fresh kit folders under test output; they do not write committed data kits or user Documents.
- Source diff counts agree with --ignore-cr-at-eol. The supervisor's pre-existing .manifest.state.json edit is untouched.
- WireUiFinalCompositionTests, gate snapshots, test census, immutable oracle, checkpoints, analyzer settings, version rings, ASG/STL and notebooks are unchanged.

# Changelog

- 2026-11-08: Wired the locked-kit read-only offer and centralized IDs; authored the two declared headless smoke facts and preserved ordinary error coverage for supervisor verification.
