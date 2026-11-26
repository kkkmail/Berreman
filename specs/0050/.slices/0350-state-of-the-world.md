# Step 0350 — state of the world

## Where we are

This step is part of spec 0050 S10.3: the staged Mueller pipeline and its reference values are promoted.
- Step 0349 promoted Stage 1 into `OpticalConstructor.Optimization.StagedMuellerPath`.
- This step makes `MuellerReconstruction.buildDesign` hand each kept row's naming-template reading to its per-row
  projection. No projection reads a label again, so the staged path that adopts `buildDesign` in step 0351 is total
  without an arm for an `Error` no kept row can give.
- Its only consumers are the two `MuellerReconstructionTests` facts, and both now take the reading.

## What's working

- `buildDesign`'s projection is `MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity`. It
  reads each label once and projects every kept row with the reading that kept it.
- The kept rows, their order and the assembled system are unchanged. The doc states the read-once contract
  (spec 0050 S10.3).
- The section 7.2 fact's projection takes φ from the handed-over reading. Its second label read and the failure on
  that read's `Error` are deleted.
- The always-run `buildDesign` fact's stub takes an ignored reading. Its rows, assertions and numbers are unchanged.
- `MuellerReconstructionTests` still passes 25 of 25 with bit-identical numbers, and constructor-unit-tests still
  passes 1555 of 1555.

## Tests

These are development runs of each gate's own command from `0350.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error` in stdout; warnings only NU1902 (exempt advisory) and the vendored MathNet SYSLIB0051 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1506 (unchanged from 0349, ELEV004 excluded) | 217 s | 600 s |
| constructor-unit-tests | Passed!, 1555 of 1555, 0 failed, 0 skipped (unchanged) | 70 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed, 0 skipped (unchanged) | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline. No test was added or removed.
- The numbers are bit-identical by construction. The reading `buildDesign` hands over comes from the same pure
  `NamingTemplate.standard.readName` call on the same label that the fact used to make itself. So every row gets the
  same φ, and every design row and target holds the same doubles.
- Both touched files are LF-only: `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1555
  berreman_unit_tests: 25
```

## Architecture

- The read-once rule lives in `buildDesign`. It reads the label, decides whether to keep the row, and passes the
  reading to the projection, all in one `List.choose`. A projection cannot see a label that the template refused,
  because such a row is never projected.
- The contract changed shape (`OpticalConstructor.Domain.MuellerReconstruction.buildDesign`, `change_shape`) and no
  other declaration changed. `NameReading` comes from `NamingTemplates`, which the module already opens. No project
  reference changed.

## Deferred

- Nothing. Every item of the brief is in place. Step 0351 is where the staged path adopts this form, as the brief
  states.

## Gotchas

- **`standardReading` stays in the test.** No projection calls it any more. The naming-template fact still uses it
  to read a literal label (`CPL-(QZ#90-LR#90)-CPL`), and that fact is no projection.
- **The doc lost its history aside.** "(the caller — slice 011 — closes `prepareRow` over …)" described history. It
  became "the reference glue's `s_eff` / `a_eff` / `signal_corrected`", so the doc describes the contract (AGENTS.md,
  Comments).

## Changelog

- 2026-11-12 — Step 0350: `buildDesign` now hands each kept row's `NameReading` to its projection, reading each label
  once (spec 0050 S10.3). The two `MuellerReconstructionTests` projections take the reading, and the 7.2 fact's second
  label read is deleted. Build, elevation, constructor-unit-tests (1555), unit-tests (`MuellerReconstructionTests`,
  25) and all 20 s35 invariants are green in development runs.
