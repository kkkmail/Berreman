# Where we are

Slice 0423 completes S121-SPLIT-GROUP-VIEW for spec 0050 S12.1, exposing step
0422's manual setup offer in the Data split panel. Groups with a name or setup
matching failure now offer the kit's setups on their existing row. All requested
implementation and test source is present. Attempt 03 repairs the null-unsafe
reason lookup identified by the supervisor's split-panel UI test diagnostic;
supervisor verification of this repair is pending.

# What's working

- Added the centralized DataSplitGroupSetup ID using the existing file-stem rule.
- Added an accessible, initially unselected setup picker to eligible unmatched rows.
- Wired setup selection to OfferSplitGroupOver and retained the existing offer toggle.
- Added the declared UI fact and extended the two specified existing facts.
- Made the unmatched-reason lookup null-safe while preserving uniqueness and color assertions.

These bullets describe delivered source, not measured test results.

# Tests

No build, test, analyzer or invariant command was run by this worker. The
IMPLEMENT worker protocol's Invariant 6 assigns execution and verdicts to the
supervisor after worker exit. The supplied attempt-03 feedback reports review
and build gates green, followed by one residual ui-tests failure at
DataStepTests.fs:503 (NullReferenceException). Its filtered retry summary
reports one failed test in 6.905 seconds against the 360-second deadline.
Attempt 03 replaces the reason lookup's unsafe StartsWith call with null-safe
equality against the full expected reason. Compilation and runtime results
after this repair remain pending; commit_ready=false does not clear the
reported UI failure. There is no worker development duration or deadline
overrun to report.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds), selected
ui-tests over OpticalConstructor.Ui.Tests.DataStepTests (360 seconds), selected
kit-acceptance over OpticalConstructor.Ui.Tests.KitAcceptanceTests (3600 seconds),
and the twenty-one s41 invariants (300 seconds each).

The sole new test identity is:

- OpticalConstructor.Ui.Tests.DataStepTests.an unreadable group's row offers the kit's setups, and choosing one sends OfferSplitGroupOver

It covers both eligible error kinds, unchanged unmatched reasons, kit-order
names, initial no-selection/no-dispatch, accessible identity, exact message
payload, absent choices on matched/offered rows, conversion into a ticked offer,
unticking, and an empty kit's unchanged reason-only row. The two named existing
facts retain their prior assertions and additionally cover the new ID (including
file-stem normalization) and the unmatched row's picker.

The expected selected UI count rises by one, from nine to ten. Constructor,
smoke and kit-acceptance test identities are unchanged. The YAML retains inherited
floors: constructor 1759 and smoke 264 from supervisor checkpoint 421; UI 9 and
acceptance 0 from the previous slice's handoff. These are not measurements of this
round. The supervisor owns measured counts and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1759
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

The new ConstructorUiId member uses FileStem.ofText for the key suffix. The
split row appends Record over: and a ComboBox only for GroupWithoutMatch with
SplitKeyNameErr or SplitKeySetupErr and a nonempty model.kitSetups. Names stay
in kit order, no entry starts selected, and a valid choice dispatches the key
and chosen SetupName through OfferSplitGroupOver. The selection handler is
resubscribed on each FuncUI patch so it uses the current setup list.

The unmatched reason and its styling stay in the existing DockPanel row.
Matched/offered rows keep their existing rendering; offered groups continue
through the existing toggle. The new control has both AutomationId and an
accessible Name. No model logic, storage format, durable shape or migration
changes are needed.

The repaired test selects the existing reason by exact text, safely ignoring
null Text values among the ComboBox's template descendants. It still requires
exactly one matching TextBlock and asserts its foreground color. No assertion
or test identity was removed or weakened.

# Deferred

No requested implementation is deferred. The supervisor must execute the
frozen gate roster before acceptance.

# Gotchas

- Only ConstructorUiIdOfDomain.fs, DataStepView.fs and DataStepTests.fs changed
  in product/test source. Ordinary and ignore-CR-at-EOL numstats agree.
- Existing automationAttribute sets only AutomationId; the picker supplies
  Name through the AttrBuilder pattern already used by the review fields.
- The new fact uses the existing folder-kit and window helpers. Bench and
  Bench (2) are transient view fixtures; offering and unticking write nothing.
- Preserve the pre-existing supervisor edits to specs/0050/.manifest.state.json
  and specs/0050/.expected-red-observed-json.
  No Git mutation, version bump, external-repo write, notebook edit, gate or
  oracle change, analyzer exemption, census change or checkpoint publication
  was performed.
- Worker documents use the exact supplied paths. No runtime artifacts were
  produced; any would belong in specs/0050/.artifacts.

# Changelog

- 2026-11-15: Added the unmatched-group setup picker, semantic dispatch and declared UI coverage; handed verification to the supervisor.
- 2026-11-15 (attempt 02): Removed the surplus closing parenthesis reported by the supervisor in the new UI fact; preserved acceptance and handed the repair back for verification.
- 2026-11-15 (attempt 03): Repaired the split-panel reason lookup's null dereference using exact-text equality; retained all assertions and handed verification to the supervisor.
