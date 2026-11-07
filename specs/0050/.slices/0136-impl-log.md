# Implementation log — step 0136

## Progress

- Read AGENTS.md, both worker prompts, the project prompt, slice 0136, its gate
  snapshot, naming conventions and the S42-OPEN-STATUS acceptance oracle.
- Inspected the existing collection-load and attach arms and their test doubles.
  CollectionLoad currently clears all statuses; attachment validation already
  delegates to ExperimentDataLoad.loadAndValidate and maps its typed errors.
- Before implementation: add the two authorized regression facts first, then
  introduce the status cases and shared validation function and wire the load arm.
- Added both authorized facts before the production change. They save and load
  through shared collection stores and assert attachment inspection, validation
  point counts, parse/range failures, no status for an unattached experiment,
  changed/missing captions, read-failure mapping and attachment preservation.
  The changed/missing mock refuses parser calls and both loading mocks refuse
  re-attachment. Tests were not executed, as required by the worker role.
- Before the production edit: extract attach-time validation without changing
  its error mapping, then derive a new status map from the loaded snapshot.
- Added DataFileChangedSinceAttach and DataFileMissingFromKit with their
  one-line contracts and exact requested captions. CollectionLoad now calls
  attachmentStatus for each attached experiment; unchanged data uses the shared
  validation helper, changed/missing data gets its specific status, and kit
  errors become AttachedFileParseError. Unattached experiments are omitted.
- Extracted the existing loadAndValidate/error mapping into the private
  validateExperimentAttachment function, called by attach and successful load.
  The replacement load comment cites both the original spec and 0050
  .manual/001 §E. All other message arms and model fields are unchanged.
- Inspected the final source diff and status consumers. Product edits are
  confined to the two authorized files; existing tests and their assertions
  remain intact. Normal and ignore-CR-at-EOL diff counts agree for both source
  files. No executable checks or gates were run.
- Completed the state-of-the-world handoff at the supplied path. No slice
  implementation requirement remains; verification belongs to the supervisor.

## Files modified

- Worker plan and this log at the task's supplied paths.
- OpticalConstructor.Ui/TableAndElementRotationView.fs: status cases, shared
  validation and status derivation on collection load.
- OpticalConstructor.Ui.Tests/InverseConstructorTests.fs: two regression facts.
- Worker state of the world at the task's supplied path.

## Testing state

No build, test, analyzer or invariant gate executed. IMPLEMENT invariant 6
assigns execution and measured results to the supervisor. Required gates:
build, elevation, ui-smoke, selected InverseConstructorTests under ui-tests,
and the eleven s16 invariants in 0136.gates. Exactly two new test identities
were added; no census or gate definitions changed. The expected selected test
count increase is two, not a measured result. The only applicable inherited
count available in .checkpoints-json is ui_smoke_tests = 215 at checkpoint 134;
no ui_tests baseline is present, so none is invented.

## Artifacts

No captured execution artifacts. Persistent artifacts, if needed, belong only
under C:/GitHub/Berreman/specs/0050/.artifacts.

## Architecture

Reused the existing injected data and collection proxies, loadAndValidate,
ExperimentDataError.fileStatus and KitRelativePath.toControlCaption. Status is
derived on open; the saved attachment path and hash remain unchanged. No durable
shape or migration changes. Changed, missing and unreadable attachments never
reach the parser on open; unchanged attachments use the same validation as attach.

## Gotchas

- specs/0050/.manifest.state.json was modified on entry; it belongs to the
  supervisor and will remain untouched.
- The role-specific prohibition on worker checks overrides the generic local
  gate-running instructions; no gate outcome will be claimed.
- No implementation deviation was needed.
