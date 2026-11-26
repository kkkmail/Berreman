# Progress

- Read the task, worker protocols, project prompt, slice and applicable standing
  rulings. Confirmed the sole new fact identity and three authorized source files.
- Before implementation: inspecting model composition, item forms, table messages,
  grid generation and storage readers to reuse the application's real paths.
- Extracted `inverseModelOver`, using `initInverseWithKit` directly over the
  opened kit (manual 010), the four-store snapshot overlay and the same app
  dependencies as the window. `studyKitIn` now calls that single composition.
- Before AT-1: adding the message-driven fixture and independent folder-reader
  assertions, including exact unknown states and byte comparisons.
- Added the sole authorized AT-1 fact, fixed kit/item identities and clock,
  real-folder CreateKit, nine item-form workflows, parameter messages, 28+12
  grid setups and both correction setups. The second proxy independently reads
  metadata, all item states, setup roles/content/angles and snapshot envelopes;
  assertions compare every library/data file's bytes and report its filename.
- Source review found the Rotate bay normalizes angles into [0, 360). The
  correction messages therefore remain -90 degrees and the persisted-angle
  expectation is 270 degrees, with the prescribed -90 setup names retained.
- Added independent required-unknown-name assertions so a missing model axis
  cannot disappear from both the construction inputs and the persisted oracle.
- Completed resource cleanup for failures during copying, app composition or
  kit construction as well as for the readback fact. The real kit closes before
  its run folder is removed.
- Completed the mandatory state-of-world handoff with inherited count floors,
  exact test identity, frozen gate selections/deadlines and the composition
  decisions. Source inspection found matching tracked numstats with and without
  ignoring CR-at-EOL. No runtime gate result is claimed.

# Attempt 02 progress

- Read the retry task and supervisor build diagnostic: FS0039 at
  `KitAcceptanceTests.fs(347,29)` could not resolve `KitHistoryLimit`.
- Before repair: source inspection located the type in
  `OpticalConstructor.Domain.WorkbenchSettings`; the new acceptance source does
  not open that module. Existing `KitStartWindowTests` imports it for the same
  default limit. Add that import only, preserving every acceptance assertion
  and the sole authorized fact identity.
- Added `open OpticalConstructor.Domain.WorkbenchSettings` to the acceptance
  source. No fixture, assertion, test identity, shared initializer or project
  registration changed in this retry. Updated the handoff to distinguish the
  supplied failed-build evidence from the pending supervisor rerun.

# Attempt 03 progress

- Read the retry task: review and build passed, but AT-1 failed on the first
  snapshot source-file existence assertion after 11.006 seconds (test run
  12.626 seconds, exit code 2), within the 3600-second acceptance deadline.
- Before repair: `DataLocations.shippedCatalogueDefault` names `Catalogue`;
  `CatalogueProxy.builtInFolderUnder` resolves its `Builtin` child. The failed
  detector entry exists there in the test output. The fixture combined each
  entry filename directly with the parent folder instead.
- Repair plan: import `Domain.CatalogueFiles` and `Storage.CatalogueFolderStore`,
  resolve the built-in folder once with the existing helper, and use it for
  every snapshot source. Preserve all file-existence, byte-equality and model
  coverage assertions, the fixture workflow and the sole fact identity.
- Applied that repair in `assertLibrarySnapshots`. Both required modules are
  imported, and every snapshot source now uses the canonical built-in folder.
  No assertion, test identity, fixture construction, product code or project
  registration changed. Updated the state-of-world with the supplied failure
  and pending supervisor verification; no scope extension was needed.
- Saved the repaired source and three worker documents as UTF-8 with LF;
  each contains zero CRLF sequences. Ordinary and ignore-CR-at-EOL tracked
  numstats match. Supervisor-owned dirty files were left untouched.

# Files modified

- Worker plan and log at the exact task-provided paths.
- `TestAppScope.fs`: shared opened-kit Inverse composition, reused by study fixture.
- `OpticalConstructor.Ui.Tests.fsproj`: appended the authorized acceptance source.
- `KitAcceptanceTests.fs`: one kit-acceptance fact, private build and readback helpers.

# Testing state

No gates run by this worker. IMPLEMENT Invariant 6 reserves build, tests,
analyzers and invariant execution for the supervisor. Attempt 03's task reports
green review and passed build gates, then a kit-acceptance failure: exit code 2,
the sole AT-1 case failed because its snapshot source path omitted `Builtin`.
The case took 11.006 seconds and the run 12.626 seconds, within the 3600-second
deadline. These are supplied supervisor results, not worker measurements.
The lookup is repaired; compilation and all gate verdicts for the repaired
source require supervisor execution after this handoff.
All requested source changes are present; `commit_ready=false` records this
verification boundary. The acceptance suite gains one identity; StudyViewTests
retains every existing assertion and identity. No allowance was changed and no
worker-run duration or deadline overrun exists because no gate ran here.

# Artifacts

No runtime artifacts produced. Any persistent output belongs under
`specs/0050/.artifacts/`.
The supplied diagnostics remain in
`specs/0050/.artifacts/0421-attempt-02-task.txt` and
`specs/0050/.artifacts/0421-attempt-03-task.txt`.

# Gotchas

- Preserve the pre-existing modifications to `specs/0050/.manifest.state.json`
  and `specs/0050/.expected-red-observed-json`.
- `shippedCatalogueFolder` is the catalogue root, not its Builtin child. Use
  `CatalogueProxy.builtInFolderUnder` when resolving shipped entry files.
- The slice's four returned values will use a named record in declared field
  order, complying with the repository's maximum-three-element tuple rule.
- The current history proxy factory accepts a present-folder list; compose it
  with the actual fixture folder rather than the obsolete unit argument in prose.
- The table has no semantic select-by-id message. Corrected-setup construction
  sets only the transient selection by resolved slot ID before sending the Rotate
  bay's `RotSetAxis`; all kit contents and persisted edits still come exclusively
  from application messages. This avoids coordinate/pointer automation and stays
  within the three authorized source files; no product message is invented.
