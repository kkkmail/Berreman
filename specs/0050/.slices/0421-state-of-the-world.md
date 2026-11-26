# Where we are

Slice 0421 implements S121-AT1-ITEMS-SETUPS, the first optics-mueller acceptance
fact. It constructs the requested kit through the start-screen and table update
paths and reads the persisted results through a second real folder proxy.
Implementation is present in the three authorized source files. Attempt 03
repairs the snapshot source lookup after the supervisor reported AT-1 looking
under Catalogue instead of Catalogue/Builtin. Supervisor verification of the
repaired source is pending.

# What's working

- Extracted the shared opened-kit Inverse initializer and reused it in studyKitIn.
- Added the sole authorized kit-acceptance fact with fixed kit/item identities and clock.
- Built nine items, their required unknowns, forty grid setups and two LR corrections through application updates.
- Asserted persisted metadata, complete item states, setup roles and contents, catalogue snapshot bytes and lab-file bytes.
- Resolved snapshot source files through CatalogueProxy.builtInFolderUnder, preserving every existence and byte-equality assertion.

These bullets describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed by this worker.
IMPLEMENT Invariant 6 assigns all gate execution and verdicts to the supervisor
after worker exit. The supplied retry task reports build exit code 1 for FS0039
at `KitAcceptanceTests.fs(347,29)` in attempt 02, repaired by importing
WorkbenchSettings. Attempt 03 reports green review and passed build gates,
then kit-acceptance exit code 2: AT-1 could not find the detector's built-in
source entry because the lookup omitted the Builtin subfolder. The failed case
took 11.006 seconds and its test run 12.626 seconds, within the 3600-second
deadline. This attempt reuses the canonical built-in-folder resolver; the
post-repair build and gate results are pending. These results are supplied
supervisor evidence, not worker measurements. The current diagnostic is in
`specs/0050/.artifacts/0421-attempt-03-task.txt`.
`commit_ready=false` reflects pending verification, not deferred implementation.
There is no development-run duration or deadline overrun to report.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all ui-smoke (600 seconds), selected ui-tests over
`OpticalConstructor.Ui.Tests.StudyViewTests` (360 seconds), selected
kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests` (3600 seconds),
and all twenty-one s41 invariants (300 seconds each).

Exactly one new fact identity is present, carrying Category=kit-acceptance:
`OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-1 builds the optics-mueller kit's items and setups through the app's messages`.
StudyViewTests and its assertions are unchanged. Expected test effects are one
new acceptance case and unchanged smoke and selected UI counts.

The smoke floor below is inherited from supervisor checkpoint 419. The UI floor
16 is inherited from the prior handoff; no ui_tests entry exists in the checkpoint
registry. The acceptance suite has no prior case or checkpoint and starts at zero;
its expected first measured count is one. These are handoff floors, not worker
measurements. The supervisor owns measured counts and checkpoint publication.

```yaml
gates:
  ui_smoke_tests: 264
  ui_tests: 16
  kit_acceptance_tests: 0
```

# Architecture

`TestAppScope.inverseModelOver` uses `initInverseWithKit` on the supplied kit,
the context's four stores overlaid with its snapshots, overlaid item models,
catalogue imports, sources, backups and export proxy, then `loadKit`. This
preserves manual 010's fallible construction rule without making a discarded
scratch kit. `studyKitIn` calls it instead of retaining a second composition.

The acceptance builder copies only the committed data tree to a fresh GUID run
folder under the test output. CreateKit performs initialization with the fixed
kit ID. Item forms supply their real offers and drafts; parameter updates,
AssignSlot, AddElement/SlideSelectedTo, grid messages, ActivateSetup, RotSetAxis
and CreateSetup own every kit-content edit. A named return record carries the
folder, app context, model and clock in the requested order without a wide tuple.
The private test fixture closes the real kit and deletes its own run folder on
success or failure. No durable contract, serializer or product behavior changed.
Snapshot assertions resolve the built-in entry folder through the existing
CatalogueProxy helper, using the same folder convention as app startup.

# Deferred

No requested implementation is deferred. Supervisor execution of the frozen
gate roster remains required before acceptance. Later AT-1 data, fitting and
publication slices are outside this step's scope and were not implemented here.

# Gotchas

- The table has no select-by-ID message. The two correction workflows resolve
  the slot's ID and set only transient selection before sending RotSetAxis.
  This documented interpretation avoids pointer automation and any product
  changes outside the authorized files; it does not author persisted content.
- The Rotate bay normalizes -90 degrees to 270 degrees. Both correction messages
  send -90, their prescribed setup names retain -90, and readback checks the
  equivalent stored 270-degree angle. Parameter bounds retain their signed radians.
- KitHistoryProxy.createInMemory currently takes a present-folder list, so the
  fixture supplies its folder instead of the slice prose's obsolete unit argument.
- Six item-model identities must occur among the snapshots; every snapshot file
  is compared with its built-in entry file, including dependent preset/category
  entries. Required unknown-name assertions also detect missing model axes.
- Source inspection found matching ordinary and ignore-CR-at-EOL numstats for
  tracked edits. Runtime correctness and compilation have not been measured here.
- Preserve the pre-existing modifications to specs/0050/.manifest.state.json
  and specs/0050/.expected-red-observed-json.
  No Git mutation, version bump, external repository change, gate/oracle/census
  amendment, checkpoint rewrite, analyzer exemption or notebook edit occurred.
- Worker documents use the exact supplied paths. No runtime artifacts were produced.

# Changelog

- 2026-11-14: Added the shared Inverse initializer and the optics-mueller item/setup acceptance fixture and fact; handed the frozen gate roster to the supervisor for verification.
- 2026-11-14 (attempt 02): Imported WorkbenchSettings to resolve the reported KitHistoryLimit compiler error; preserved the fixture and assertions and returned verification to the supervisor.
- 2026-11-14 (attempt 03): Resolved snapshot sources under Catalogue/Builtin with the existing folder helper; preserved all acceptance assertions and returned verification to the supervisor.
