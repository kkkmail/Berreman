# Progress

- Read the task, shared and IMPLEMENT worker protocols, project prompt,
  AGENTS.md, naming conventions, applicable manuals 013/015, the frozen slice
  and gate snapshot, and the S121-SPLIT-GROUP-OVER oracle requirement.
- Inspected DataSplit and its existing domain/model tests. The existing
  SetupOfferForGroup/MatchByTemplate path already owns experiment creation and
  attachment; the new operation only needs to create and tick a valid offer.
- Before the test step: add the three declared identities and extend the
  existing rendering fact, reusing the local kit fixtures.
- Test source added: the two domain facts cover a real Phone ROI CSV, both
  eligible error kinds, free-name suffixes, preserved groups and every refusal
  category. The existing rendering fact now checks descriptions and captions.
- The sole new model fact reuses the real folder kit, creates Bench through
  messages, offers Darkness, checks no-write/refusal/read-only behavior and
  name reservations, then asserts the persisted experiment and keyed rows.
  Existing identities and assertions remain intact; no tests were executed.
- Before the implementation step: add the two errors and pure offering member,
  then wire the model message to it without changing MatchByTemplate.
- Implemented DataSplit.withGroupOver with typed unknown/readable refusals,
  accepting only name/setup failures and reusing key.nameText.toExperimentName
  and nextFreeAmong. All other groups remain in place.
- Added OfferSplitGroupOver, resolving the requested held setup, reserving kit
  and other-offer names and ticking the new offer without IO or a write guard.
  MatchByTemplate and its write helpers are unchanged.
- DataSplitError.caption already delegates directly to describe, so it needs
  no second match; its documentation now names the new refusal contract and
  the widened existing test covers both new captions through that delegation.
- Before the handoff step: inspect the source diff and naming/style consistency,
  preserve LF, and write the required state-of-the-world with inherited floors.
- Source review completed: the diff changes only the five authorized source
  files, alongside the worker documents and the untouched pre-existing
  supervisor edit. Ordinary and ignore-CR-at-EOL numstats agree, and all five
  edited source files contain zero carriage-return bytes.
- Wrote 0422-state-of-the-world.md. All requested implementation is present;
  compilation and runtime behavior await the supervisor's frozen gate roster.

# Files modified

- 0422-impl-plan.md and this log.
- OpticalConstructor.Tests/DataSplitTests.fs.
- OpticalConstructor.Tests/DataSplitModelTests.fs.
- OpticalConstructor.Domain/DataSplits.fs.
- OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs.
- OpticalConstructor.Ui/TableAndElementRotationView.fs.

# Testing state

No gate commands have been run. IMPLEMENT Invariant 6 reserves execution and
verdicts for the supervisor. The required source tests will be delivered for
that execution. No count, deadline, selection, oracle or census is changed.

Exactly three new Fact identities were added, matching the slice's effects;
the existing split-error fact was extended without removing its assertions.
The expected constructor delta is +3 (1756 inherited to 1759 expected), with
UI and acceptance counts unchanged. These are source expectations, not test
results. No measured duration or deadline overrun exists in this worker round.
The handoff uses commit_ready=false solely because supervisor verification is
pending; no implementation requirement is deferred.

# Artifacts

No runtime artifacts produced. Persistent artifacts, if needed, belong only in
C:/GitHub/Berreman/specs/0050/.artifacts.

# Gotchas

- The working tree initially contains a supervisor edit to
  specs/0050/.manifest.state.json; preserve it.
- The UI picker itself belongs to step 0423; this slice adds its semantic
  message and model behavior only, as the executable scope specifies.
- The caption contract was already a delegation to describe, so no exhaustive
  match needed widening. Updating its documentation and exercising every error
  caption preserves the existing single rendering path.
- The model collision subcase constructs only transient state to represent a
  split whose names have become reserved. It performs no writes; the persistence
  subcase uses the real setup created by messages and the original split.
