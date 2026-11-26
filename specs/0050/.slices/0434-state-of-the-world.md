# Where we are

Step 0434 lands the third part of AT-2, the absorbing-biaxial-noisy acceptance kit (spec 0050 S12.2; 001 §Q AT-2). Steps
0432 and 0433 built the library drop, the items, the table, the five setups and one experiment per plate. Now the build
writes the harness's noisy observations for seeds 1–5 into the kit's `data/` through the Mueller-matrix observation
file, then splits seed 1's file by `sample` and attaches it in place to the five experiments. Steps 0435–0439 add the
study, the fit, the ensemble over the other four files and the committed-kit equality.

# What's working

- AT-2's build writes `data/noisy-observations-seed-1…5.csv` through `KeyedObservation.observationFileText`.
- Each observation is keyed by its plate's label, and the seeds are drawn as `AbsorbingBiaxialNoisyTests` draws them.
- Seed 1's file is split by `sample` (five `ExperimentForGroup` matches) and attached in place by `MatchByTemplate`.
- Added the fact proving the five files byte-identical to regenerated text and read back element by element.
- The same fact proves each experiment's keyed seed-1 attachment at its SHA-256, and the window's read of each plate.

# Tests

These are development runs, evidence only and not gate verdicts; the supervisor's gates are authoritative.
- The UI test project builds with 0 errors. The only warnings are the exempt NU1902 advisory, plus the vendored MathNet's
  existing `SYSLIB0051` warnings when it is rebuilt.
- kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 9 of 9 succeeded, up from 8, in 84 s
  against 3600 s. The TRX names the new fact by its census id, `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2
  writes each seed's noisy observations through the Mueller-matrix observation profile and attaches seed 1's to its
  plates`.
- ui-smoke over every discovered smoke test: `Passed!`, 264 of 264, in 238 s against 600 s.
- ui-tests selecting `StudyViewTests` and `DataStepTests` (both untouched): `Passed!`, 18 of 18, in 28 s against 360 s.
- elevation was not run: the only edited file is in a test project, outside the ELEV scope.
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.

No development run exceeded its effective deadline. Following Invariant 6, the final JSON reports no gate result, so
`commit_ready` is false only because the worker runs no gate. No requirement or authority is blocked.

```yaml
gates:
  kit_acceptance_tests: 9
```

# Architecture

Everything is in `KitAcceptanceTests.fs`.

- **Harness data.** `noisyObservationsOf` (the seed's `noisyObserveAbsoluteWith` draw) and `plateLabelOf` (step 433's
  plate table read from cut to plate) feed `plateKeyedObservationsOf`. That is the one keying body the build and the
  fact share. `observationFileOf` turns its output into the file through `KeyedObservation.observationFileText`.
- **The build.** `attachNoisyObservations` writes the five files as the bench's instrument would, beside AT-1's lab
  files. It then drives the window's own `SplitByColumn` and `MatchByTemplate` messages and asserts each stage.
- **The fact.** Two checks:
  - `assertNoisyObservationFiles`: the exact file set, bytes, and the profile read-back;
  - `assertNoisyObservationAttachments`: the second proxy's attachments and the window's `readMapped`.

  Both compare through `assertSameObservations`: key, configuration and `measured.element i j`, exactly.

# Deferred

Nothing is deferred. Steps 0435–0439 add AT-2's later parts, as the spec plans: the ensemble attaches the other four
files in turn (0437), and the committed-kit equality compares them (0438).

# Gotchas

- Exact readback rests on `T·((T·x)/T)` round-tripping, because the file stores normalized elements and the throughput.
  It holds for every element of the five seeds, and a 10⁶-pair probe found no counter-example
  (`.artifacts/0434-probe/`).
- In file order the `OpticAxisCut` rows are not contiguous (A1's four, then `OtherOpticAxisCut`, then A2's twelve).
  `KeyedDataRows` keeps file order, which matches the plate's scan order from step 433.
- `MatchByTemplate` needs no draft capture when every group matches an existing experiment.

# Changelog

- 2026-11-15: Step 0434, AT-2's third part. The build writes the five seeds' observation files into `data/`, splits seed
  1's by `sample` and attaches it in place. Added the fact checking the file set, the bytes, the exact read-back, each
  keyed attachment and the window's read.
