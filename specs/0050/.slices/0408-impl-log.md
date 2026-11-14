# Step 0408 implementation log

## Progress

- Read the task, both worker protocols, project prompt, slice, repository
  instructions, naming conventions, applicable rulings 013/015 and preceding
  handoff. Inspected the existing promotion, import and snapshot seams.
- Before test changes: add exactly the three authorized persistence facts using
  the existing pure promotion fixtures, and extend the error-rendering fact.
- Added those three facts over fresh disposable databases and initialized
  in-memory kits. Assertions cover latest version, original version preservation,
  fit provenance, crystal/custom-Mueller payloads, pins, snapshot envelopes and
  bytes, and read-only no-IO refusal. Extended the existing error-rendering fact
  with all three new errors, preserving its earlier assertions.
- Before product changes: share the version lookup, content construction, seal,
  write and snapshot sequence in one private function; add exactly the two
  prescribed members, each guarding read-only access before the pure promotion.
- Implemented `promoteMaterial` and `promoteBlackBox`, the three typed errors and
  their descriptions. The private shared sequence uses `held.latest.next` or
  `VersionNumber.first`, seals fit-provenance content, writes with
  `MoveLatestVersion`, and returns the exact reference only after its snapshot.
- Inspected the source diff and existing API declarations. Adjusted new record
  expressions to the required layout and annotated the read-only error result.
  The existing material content constructor is total; the preset constructor's
  error is mapped without changing either API.
- Wrote the required state-of-the-world at the exact supplied path, documenting
  implementation, pending verification and the preserved measured baseline.
- Normalized the five worker-owned files to LF. Ordinary and CR-ignoring source
  diff statistics match (Domain +69/-1, tests +276/-2). Final source/status
  inspection shows only the two authorized source edits, three worker documents
  and the inherited supervisor manifest change. No executable checks were run.

### Attempt 02 repair

- Read the retry task and supervisor diagnostics: build failed with FS0039 at
  the fixture's `MaterialError.describe` call and three FS0046 warnings for the
  local `component` identifier. The supplied build elapsed time was 78.79 s,
  within the unchanged 600 s deadline; this is a failed supervisor result.
- Before repair: inspected `MaterialError` and the existing `StudyMessageTests`
  material-save fixture. The error type has no renderer. Reuse the typed
  `Assert.Equal<Result<unit, MaterialError>>(Ok (), ...)` save assertion and
  rename the Mueller component binding without changing its assertions.
- Preserve the previous implementation and all authorized test identities.
  No gate is run by this worker, under IMPLEMENT Invariant 6.
- Replaced the invalid renderer call with the existing typed save-result
  assertion. Renamed the local binding and its two uses to `muellerComponent`;
  the original angle and matrix-element assertions are unchanged. No product
  code or test identities changed during this repair.
- Updated the plan and state-of-the-world at their designated paths to record
  the retry, the supplied failed-build evidence and pending supervisor gates.

## Files modified

- Created the implementation plan and this log at the designated paths.
- `StudyPublishingTests.fs`: three persistence facts, local fixture support and
  the widened error-rendering fact.
- `StudyPublishing.fs`: the two public promotion members, shared private mint
  sequence and three errors with diagnostic rendering.
- `0408-state-of-the-world.md`: the required handoff with all structural sections.

## Testing state

No gates or executable checks run. The IMPLEMENT worker protocol reserves gate
execution and measured verdicts for the supervisor after worker exit.
The attempt-02 task supplies a failed supervisor build (exit code 1, elapsed
78.79 s against a 600 s deadline). Its FS0039 and three FS0046 diagnostics are
addressed in source; no fresh build or passing result is claimed. The supplied
log also includes NU1902 package advisories and two SYSLIB0051 warnings in the
vendored MathNet sources, outside this slice's authorized edits.
Checkpoint `406` records 1735 constructor cases. The three added facts imply
1738 expected cases; that is not a measured result. The pending roster is build,
elevation, whole-suite constructor tests and the twenty s40 invariants, with
unchanged deadlines and selection. No gate readiness is claimed.

## Decisions and gotchas

- Preserve the pre-existing `specs/0050/.manifest.state.json` modification.
- The read-only guard must precede pure promotion and all library/kit IO, since
  the library write would otherwise precede snapshot refusal.
- `LibraryEntryContent.ofMaterialEntry` returns content directly; only
  `ofPolarizerPreset` returns a typed construction error. Preserve those existing
  signatures and lift material content into `Ok` for the shared sequence. No new
  public API or manufactured material-construction error is needed.

## Artifacts

No runtime artifacts produced by this worker. The supplied retry diagnostics
are in `specs/0050/.artifacts/0408-attempt-02-task.txt`. Persistent artifacts,
if needed, belong only in `specs/0050/.artifacts/`.
