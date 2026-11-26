# Implementation log — 0135

## Progress

- Read AGENTS.md, task, shared and IMPLEMENT worker protocols, project prompt,
  slice, resolved gates, naming conventions and previous handoff reports.
- Entry worktree has a supervisor-owned specs/0050/.manifest.state.json edit;
  leave it untouched.
- Before implementation: trace the existing attachment types, kit proxy methods,
  measured-data seam, UI consumers and authorized fixture identities. Reuse
  existing kit operations and parser behavior without changing their contracts.
- Before fixture edits: adapt existing assertions to kit attachments and add only
  the two storage facts and one UI fact explicitly authorized by this slice.
- Implemented the domain/storage contract switch and UI consumers. DataFilePath
  is retired, KitFileText hands text to the existing parser, and the storage
  adapter reuses KitProxy for attachment, status and relative-path reads. Picked
  byte reads map exceptions to KitIoErr at the IO boundary. UI failure status
  preserves the current collection; successful attachments still report parser
  and range-validation outcomes. Both composition sites use a session kit.
- Fixture edits in progress: retain all existing assertions, update exact load
  signatures and collection fixtures, and cover unreadable picks without adding
  any test identity beyond the three authorized facts.
- Completed fixture updates, two prescribed renames, two new storage facts and
  one new UI fact. Existing load assertions remain; added exact kit-read error
  diagnostics and four-field signature assertions. The in-place storage case
  also uses the read-only view and compares names, bytes and last-write time.
  The UI failure case also checks replacement preserves an existing attachment
  and never invokes either loader after attachment fails.
- Source review found the storage-defined KitProxy.createInMemory extension
  needs its owning module opened at both composition sites; added those opens.
  Reviewed consumers, diff and line-ending statistics without executing gates.
  The 23 changed product/test files match the authorized edit list. Ordinary
  and ignore-cr-at-eol numstats agree for these files; the pre-existing manifest
  change remains supervisor-owned.
- Before handoff: record inherited checkpoint values (constructor 1,092 at 133;
  UI smoke 215 at 120), write the mandatory state-of-world, and complete this
  log. There is no persisted ui_tests count; selected results remain the
  supervisor's responsibility. Expected constructor increase is two cases and
  selected UI increase is one; these are not measured results.

### Attempt 02

- Read the retry task and worker protocols. The supervisor reports ELEV013 at
  ExperimentDataStore.fs:35 and requests RequireQualifiedAccess on the module
  while preserving the public factory signature and existing behavior.
- Before repair: inspected NoSingleParameterFunction.fs in the read-only STL
  source. Its public-function rule explicitly sanctions RequireQualifiedAccess
  modules. Existing production and test call sites already qualify the factory.
  Add only the module attribute, then update this log and the state-of-world;
  do not change test identities, analyzer configuration or the findings ledger.
- Applied RequireQualifiedAccess to ExperimentDataStore. The factory signature,
  function bodies and callers are unchanged from attempt 01. This is the only
  product edit in attempt 02; the three handoff documents record the repair.
  No test, analyzer configuration, findings-ledger or supervisor-state edits
  were made in this attempt. Gate verification remains with the supervisor.

### Attempt 03

- Read the retry task, worker protocols, project prompt, slice, gate snapshot,
  naming conventions and existing handoff documents. The supervisor reports
  one ELEV001 finding at SolverHandoffView.fs:89:30: constructing ControlCaption
  directly from KitRelativePath.value at the call site.
- Before repair: place the one-way conversion on KitRelativePath as an extension
  in ConstructorUiIdOfDomain. This authorized file already owns Domain-to-Controls
  conversions and compiles before SolverHandoffView. Update handoffFileCaption
  to call the member, keeping the display and missing-file branch unchanged.
  Preserve prior implementation, tests and supervisor-owned state files.
- Applied KitRelativePath.toControlCaption in ConstructorUiIdOfDomain and used
  it from handoffFileCaption. The conversion now belongs to its source type;
  its output remains the attachment's relative path. This repair changes only
  those two authorized UI files and these handoff documents. No test identities,
  assertions, analyzer rules, findings-ledger entries or supervisor state changed.
- Reviewed the final source diff and declaration order. The named conversion has
  one declaration and one caller; ordinary and ignore-cr-at-eol numstats agree
  for the two UI files. All five files written in this attempt use LF. No gates
  or test commands were executed.

## Files modified

- Created the prescribed implementation plan and this log.
- Domain: Experiments.fs, MeasuredData.fs, ExperimentDataProxy.fs,
  ExperimentDataLoad.fs, ExperimentCollectionStore.fs and the Domain project
  comments for attachment contracts and parser handoff.
- Storage: ExperimentDataStore.fs for kit-backed loads, attach and status.
- UI: AppContext.fs, ConstructorUiIdOfDomain.fs,
  TableAndElementRotationView.fs, UserEnvironment.fs, SolverHandoffView.fs and
  SolverHandoffWindow.fs for composition, errors, picker and relative captions.
- Constructor tests: ExperimentDataProxyTests.fs, ExperimentDataStoreTests.fs,
  ExperimentProxyTests.fs, ExperimentCollectionProxyTests.fs,
  ExperimentCollectionRoundTripTests.fs and project comments.
- UI tests: InverseConstructorTests.fs, DataFilePickerFolderTests.fs,
  SolverHandoffWindowTests.fs and ScrollExtentTests.fs.
- Created the prescribed 0135-state-of-the-world.md.

## Testing state

No worker checks or gates run. IMPLEMENT invariant 6 assigns execution and
acceptance to the supervisor. Test identities and selection remain governed by
0135.slice-md and 0135.gates; no census or baseline will be edited.

Attempt 02 addresses the supervisor-reported ELEV013 finding with the requested
module shape. No post-repair gate result or measured count is claimed.

Attempt 03 addresses the supervisor-reported ELEV001 finding with the named
source-type conversion requested by the diagnostic. No post-repair gate result
or measured count is claimed; execution remains assigned to the supervisor.

The supervisor runs build, elevation, constructor-unit-tests, ui-smoke,
ui-tests and the eleven s16 invariants. All implementation is complete;
gates_run and gate_results remain empty. Under the act-only handoff convention,
commit_ready is true to submit the completed work for supervisor verification,
not to assert passing gates.

## Artifacts

Persistent artifacts belong only in specs/0050/.artifacts. No execution
artifacts were created.

## Gotchas

- The role-specific act-only protocol supersedes the base protocol's legacy
  self-check instructions. Source inspection is not a passing-gate claim.
- No scope deviation identified.
- Attempt 02 follows the explicit retry instruction to use RequireQualifiedAccess
  for the ELEV013 public-function contract; it does not resolve or waive a name
  collision, and it changes no analyzer rule.
- Attempt 03 keeps the conversion in the existing UI extension module so Domain
  gains no dependency on Controls. Both changed source files are in the declared
  effects; there is no authority conflict.
- The retired path type's two Mueller history notes are expressly retained by
  the slice; those files were not edited. No compatibility reader was added.
