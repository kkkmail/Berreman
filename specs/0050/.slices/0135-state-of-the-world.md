# Where we are

Step 0135 implements S42-DATA-SEAM after step 0134 supplied kit attachment and
status operations. Experiments now carry DataAttachment, and their measured
data is loaded through the owning kit. All authorized production and fixture
changes are implemented; verification and acceptance belong to the supervisor.
Attempt 02 adds RequireQualifiedAccess to ExperimentDataStore in response to the
supervisor's ELEV013 finding, preserving the requested factory signature.
Attempt 03 moves the handoff caption conversion onto KitRelativePath in response
to the supervisor's ELEV001 finding, preserving the displayed relative path.

# What's working

- Replaced absolute experiment data paths with kit-relative attachments and hashes.
- Routed measured-data attach, status and parser reads through the supplied kit.
- Preserved existing attachments on failure and surfaced a typed UI status.
- Updated picker folder memory, session composition and relative-path captions.
- Adapted existing tests and added the three authorized attachment cases.
- Repaired the kit factory's module shape and the handoff's source-type conversion.

# Tests

No worker build, test, analyzer or invariant gate was run, per IMPLEMENT
invariant 6. The supervisor owns build, elevation, constructor-unit-tests,
ui-smoke, selected ui-tests and the eleven s16 invariants in 0135.gates.
No gate success or newly measured count is claimed.
The current retry repairs the supervisor-reported ELEV001 finding at the handoff
caption; its post-repair result remains pending supervisor execution. Test
identities, assertions, analyzer configuration and the empty findings ledger
are unchanged this attempt.

The two new storage facts cover outside copy and in-place attachment, plus
unreadable picks leaving the kit unchanged. Existing parser tests now exercise
real kit-relative reads. The new UI fact covers failure with no attachment,
failed replacement preserving the prior attachment, diagnostic captions and
no load after failed attachment. All existing assertions remain, adapted to
DataAttachment or FileName, with exactly the two authorized test renames.

The following counts are inherited from supervisor checkpoints 133
(constructor) and 120 (UI smoke). The expected constructor increase is two
cases to 1,094, and the selected UI increase is one case; these are unmeasured
expectations. The checkpoint file has no ui_tests baseline, so none is invented.

```yaml
gates:
  constructor_unit_tests: 1092
  ui_smoke_tests: 215
```

# Architecture

ExperimentDataProxy captures one KitProxy. It reuses attachLabFile,
attachmentStatus and readDataFile; direct IO is limited to reading the picked
file's bytes and translating an exception to KitIoErr. Kit read errors remain
in the existing four-case measured-data channel as MalformedDataFileErr.
KitFileText.dataFileText is a one-way conversion; parsing and validation remain
unchanged. The two UI composition sites own session-local in-memory kits.

ExperimentDataStore now requires qualified access, as directed by the retry.
ELEV013 explicitly sanctions its public function in this module shape; existing
callers already qualify createFileBacked. The factory still takes KitProxy and
returns ExperimentDataProxy, with no behavior or call-site changes.

KitRelativePath.toControlCaption lives in ConstructorUiIdOfDomain, where both
Domain and Controls types are available. The handoff calls this one-way
source-type conversion instead of unwrapping and rewrapping at the call site;
Domain gains no UI dependency and the displayed text is unchanged.

Experiment.dataFileOpt holds a relative path and its recorded content hash.
No absolute-path compatibility shape or migration was retained. FileName is
used only for the pick and folder-memory boundary. Failure to attach leaves the
experiment collection unchanged; successful attachment is retained even when
parsing or validation subsequently fails.

# Deferred

None of the requested implementation is deferred. Gate execution is assigned
to the supervisor by the worker protocol.

# Gotchas

- KitProxy.create and createInMemory are Storage extension members; their
  composition sites must open OpticalConstructor.Storage.KitFolderStore.
- The two Mueller history notes retain the old path-type name as explicitly
  permitted by this slice; neither Mueller file changed.
- The supervisor-owned manifest-state edit was present on entry and untouched.
- The supervisor-owned expected-red state edit was also present on attempt 03
  entry and untouched.
- Source inspection and LF-preserving edits are not substitutes for the
  supervisor's pending build, tests and analyzers.

# Changelog

2026-11-07 — Replaced the measured-data absolute-path seam with kit attachments,
updated UI behavior and fixtures, and added the prescribed storage/UI coverage.
2026-11-07 — Attempt 02: added RequireQualifiedAccess to ExperimentDataStore for
the reported ELEV013 finding, preserving the factory and existing test surface.
2026-11-07 — Attempt 03: added KitRelativePath.toControlCaption and called it from
the solver handoff for the reported ELEV001 finding, preserving captions and tests.
