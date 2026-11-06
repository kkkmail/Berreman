# Implementation log — 0052

## Operator-authorized repair and verification — 2026-11-06

- Operator ruling: `.manual/004-step-0052-catalogue-test-rename.md` authorizes
  the single PropagationTests declaration rename, preserving its body and count.
- Renamed the fact to `the curated Library contains exactly the catalogue samples`.
  A byte comparison against HEAD proves this is the only source change.
- Added the step-52 rename to `.operator-test-effects-jsonl`. Temporal replay
  retains the old identity through step 51 and the new identity from 52 through
  494, with no suite-count change or replay findings.
- The production compiler validated the plan and refreshed `.executability-json`,
  preserving all 494 existing descriptors and gate snapshots. The only receipt
  change binds the operator test-effects file. Receipt validation is green.
- Both unchanged step-52 acceptance searches returned no matches.
- All ten resolved gates passed through ASG's evaluator at 10:34:38–10:35:55 UTC:
  build, elevation, constructor-unit-tests and all seven s07 invariants.
- Constructor tests: 805 passed, zero failed/skipped. Exact identity reconciliation
  passed; the renamed fact passed and the old identity is absent.
- Build: zero errors and no warnings from our code. Two existing SYSLIB0051
  warnings originate in vendored MathNetNumerics/Numerics/Exceptions.cs:24,77.
  Elevation correctly reported no in-scope product project touched by test edits.
- Evidence is in `.artifacts/0052-operator-repair/`: complete verification log,
  per-gate stdout/stderr, gate-results.json, test-inventory.json, saved TRX,
  acceptance-searches.json and contract-validation.json.
- Ready for the operator's commit. No git mutation, arc restart, acceptance,
  state/checkpoint update, published-spec edit or gate change was performed.

The sections below retain the original pre-ruling worker attempt as history.

## Initial progress

- Read the task, worker protocols, project prompt, repository instructions,
  naming conventions, slice, resolved gate roster, and existing catalogue fixture.
- Before implementation: inspect the nine authorized files and identify seed
  references, positional selection, store construction, and ordering assumptions.
- Replaced executable seed references with the fixture lists, by-id sample
  lookups, stable ids, material/sample stores, and catalogue libraries.
- Kept the existing VersionsInUse arguments and material-reference callbacks.
  Material properties in the Langasite comparison now come from the fixture.
- Before final source adjustments: update the Sources tree comparison, placed
  sample ids, obsolete imports and seed/store documentation; review the diff.
- Completed the Sources tree comparison using its existing recursive leaf
  collector and sorted ids on both sides, preserving duplicate detection.
  Other source-offer comparisons retain their existing ordered semantics.
- Placed-sample bindings now use the three SampleIds.toElementValueId values.
  ExperimentRunTests loads the four named tunable PresetIds through source.
- Removed unused MaterialStore, SampleStore and MaterialLibrary opens where
  appropriate, and replaced obsolete seed/store documentation with fixture names.
- Reviewed the source diffs: no facts, assertion calls or numeric expectations
  added or removed. Existing list positions select generated results or scene
  elements, not catalogue seeds, and stay unchanged.
- Ordinary and CR-insensitive source diff counts agree for all nine files.
  The unrelated supervisor manifest change remains untouched.
- Wrote the required state-of-the-world at its canonical path. Implementation
  is complete except for the contradictory acceptance/name obligations below.

## Files modified

- Worker plan and this log at the task's canonical paths.
- All nine test files named by the slice; no product or fixture file.

## Initial testing state (historical)

No checks run. IMPLEMENT Invariant 6 reserves gate execution for the supervisor.
The resolved roster is build, elevation, constructor-unit-tests, and seven s07
invariants. Gate results and discovered test counts remain unmeasured this round.

Source review is not gate execution. The fact-name/text-oracle conflict in
Gotchas prevents certifying the entire slice as accepted. `commit_ready` is
false; no gate failure or successful result is claimed. The inherited count of
805 in the preceding 0051 handoff is carried forward as historical evidence,
not a new baseline or a measured result.

## Artifacts

No captured run artifacts; no gates executed.
The implementation plan, this log and the state-of-the-world are written only
at the three paths supplied in the task.

## Initial gotchas (name conflict resolved by the ruling above)

- `specs/0050/.manifest.state.json` was already modified on entry; leave it alone.
- Preserve fact names even when they use historical seed terminology.
- Fixture source row order differs from the derived Sources tree's group order.
- Requirement conflict found in `PropagationTests.fs`: the existing fact name
  `the curated Library seeds exactly the named SeedSamples` contains a word the
  acceptance grep forbids, while the implementation brief says "Every fact keeps
  its name and its assertions". Preserve the fact name and complete its body
  conversion; no source encoding workaround, test rename, or gate edit is
  authorized. The supervisor/operator must resolve whether to exempt that name
  from the text oracle or authorize renaming that one fact. This is a source-level
  contradiction, not a reported gate failure; no acceptance grep was executed.
