# Where we are

Slice 0422 implements S121-SPLIT-GROUP-OVER for spec 0050 S12.1. Unreadable
lab-data groups can now be offered over a setup chosen from the kit, with an
experiment named from their key and made free among reserved names. The offer
is ticked for the existing MatchByTemplate application path. All requested
source changes are present; supervisor verification is pending.

# What's working

- Added typed unknown-key and ineligible-group refusals with descriptions and captions.
- Added DataSplit.withGroupOver for naming/setup failures, preserving all other groups.
- Added OfferSplitGroupOver to resolve a kit setup, reserve experiment names and tick the offer without IO.
- Added the three declared regression facts and extended the existing error-rendering fact.
- Covered keyed persistence through MatchByTemplate, refusal/no-write behavior and read-only offering.

These bullets describe delivered source, not measured gate results.

# Tests

No build, test, analyzer or invariant command was run. The IMPLEMENT worker
protocol's Invariant 6 assigns execution and verdicts to the supervisor after
worker exit. Compilation and runtime results are therefore pending, and
commit_ready=false does not claim a gate failure or defer implementation.
There is no development-run duration or deadline overrun to report.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds), selected
ui-tests over OpticalConstructor.Ui.Tests.DataStepTests (360 seconds), selected
kit-acceptance over OpticalConstructor.Ui.Tests.KitAcceptanceTests (3600 seconds),
and all twenty-one s41 invariants (300 seconds each). No selection, deadline,
test census, checkpoint, acceptance oracle or gate descriptor was changed.

The three new identities are exactly:

- OpticalConstructor.Tests.DataSplitTests.a group the template cannot read is offered over the setup the user names, its experiment named by its key
- OpticalConstructor.Tests.DataSplitTests.only a group without a readable setup is offered by hand, and an unknown key is refused
- OpticalConstructor.Tests.DataSplitModelTests.offering an unreadable group over a kit setup ticks it, and applying the split records its experiment under the key's name with the key's rows

The existing `every split error renders its condition and its payload` fact
keeps its assertions and also covers the new descriptions and every caption.
The expected constructor delta is +3, from inherited 1756 to expected 1759.
Smoke, selected UI and acceptance test identities are unchanged.

The YAML carries inherited floors, not measurements of this round: constructor
1756 from supervisor checkpoint 419, smoke 264 from checkpoint 420, and the
DataStepTests selection's nine cases documented in the 0328 handoff. Acceptance
retains the prior 0421 handoff's zero floor because the checkpoint registry has
no acceptance count; the existing selected acceptance fact is unchanged. The
supervisor owns measured counts and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1756
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

DataSplit.withGroupOver accepts only GroupWithoutMatch with SplitKeyNameErr or
SplitKeySetupErr. It uses the existing key.nameText.toExperimentName conversion
and ExperimentName.nextFreeAmong, replacing that group's matching with a
SplitSetupOffer over the supplied setup. An absent key is SplitKeyUnknownErr;
every ineligible group is SplitKeyReadableErr. Name conversion errors remain
typed. The split and offers are transient; no durable shape or migration changes.

OfferSplitGroupOver resolves the setup from model.kitSetups, reserves all kit
experiment names and every other offer's name, then updates only the held split
and its creating keys. It writes nothing and therefore needs no read-only
guard. Missing split/setup and domain refusals change only kitStatus.
MatchByTemplate and its helpers are unchanged: they own attachment, setup
recording when needed, and the new experiment's scan and keyed rows.

DataSplitError.caption already delegates to describe. That delegation renders
the new cases without another match or a duplicate rendering implementation.

# Deferred

No requested implementation is deferred. The supervisor must execute the
frozen gates before acceptance. The setup picker belongs to the separately
specified step 0423 and is outside this slice's effects.

# Gotchas

- The model test reuses its existing folder-kit fixture and creates Bench by
  semantic messages. It checks both source and copied file bytes, exact keyed
  attachment, persisted setup/experiment data and preserved draft/table state.
- A transient collision subcase reserves Darkness in the experiment collection
  and Darkness (2) in an unticked other offer; offering must produce Darkness (3).
  This subcase writes nothing and does not replace the real persistence fixture.
- Source inspection found matching ordinary and ignore-CR-at-EOL numstats and
  zero carriage returns in every edited source file. This is not build evidence.
- Preserve the pre-existing supervisor edit to specs/0050/.manifest.state.json.
  No Git mutation, version bump, notebook change, external-repository write,
  analyzer exemption, gate rewrite or checkpoint publication occurred.
- Worker documents use the exact supplied paths. No runtime artifacts were produced.

# Changelog

- 2026-11-14: Implemented manual setup offers for unreadable split groups, wired the semantic message and authored the three declared regression facts; handed verification to the supervisor.
