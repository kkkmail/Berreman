# Step 0050 state of the world

## Where we are

Spec 0050 S1.7 step 0050 is implemented. The dark-line experiment seed is
retired, the in-memory proxy is empty and its tests describe that contract.
The diagnostic test owns its dark-line fixture. Acceptance awaits the
supervisor's gate engine; no successful gate result is claimed.

## What's working

- Delete the experiment seed and return empty listing and lookup results.
- Replace the two seed facts with empty-proxy facts covering ids 1 and 9999.
- Construct the diagnostic dark line locally with unchanged assertions.
- Preserve the proxy signature and the number of test facts.

## Tests

No build, test, analyzer or invariant gates were run, per Invariant 6 and the
IMPLEMENT worker protocol. The supervisor's resolved roster is build,
elevation, constructor-unit-tests (all discovered), and the seven s07
invariants: elev-list-empty, no-vstest, no-kit-kit-json,
no-eps-axis-evaluated, complexity-mandatory, no-seeded-string-preset-ids and
no-seeding-seam.

Source review against the immutable S17-NO-SEED-EXPERIMENTS obligation found
the specified deletion, empty results, exact replacement fact names and
test-local fixture in the three-file diff. The diff replaces two facts with
two facts and leaves diagnostic assertions intact. Ordinary and CR-insensitive
source diff counts agree. These observations are not executed gate results.

The count below is inherited from checkpoint 46, the latest recorded
constructor-suite count. It is not a measurement from this round; the
supervisor owns the baseline and subsequent discovery and execution.

```yaml
gates:
  constructor_unit_tests: 805
```

## Architecture

createInMemoryExperiments retains unit -> ExperimentProxy and directly returns
Ok [] for listExperiments and Ok None for tryGetExperiment. No seed collection
or lookup closure remains. Its one-line documentation states the empty
contract and cites spec 0050 S1.7.

OutOfBandDiagnosticTests constructs the retired template's Experiment inside
the existing fact: id 1, empty setup, no variation, CaptureTransmitted,
VariableRange.forVariable VaryR1, no data file and no source wavelength.
No persisted shape changes, migration, fallback, helper or new type is added.
There is no deviation from the slice.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor's gate engine as required by the worker protocol.

## Gotchas

- The incoming specs/0050/.manifest.state.json modification is supervisor-owned
  and remains untouched.
- The two replacement facts have exactly the names specified in the slice;
  the lookup fact checks both the former seed id and an unknown id.
- commit_ready is false solely because the worker cannot certify pending
  supervisor gate results; the requested implementation is complete.

## Changelog

- 2026-11-06: Removed the experiment seed, replaced its proxy assertions and
  moved the diagnostic dark-line fixture into its existing fact.
