# Step 0350 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md, `MuellerReconstruction.fs:buildDesign` and its two
      consumers in `MuellerReconstructionTests.fs`.
- [x] `buildDesign` hands each kept row's reading to `prepareRow`.
- [x] The two facts' projections take the reading.
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (selected class) and the 20 s35
      invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`, `buildDesign` only:
  - the first parameter is `prepareRow : MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity`;
  - `isOfKind`, `kept` and the separate `List.map prepareRow` are replaced by one `List.choose`. It reads each row's
    label once through `NamingTemplate.standard.readName`. On `Ok reading` whose `matrixKind = kind`, on a label with
    no `(AIR#0)`, and on a row that is not an excluded point, it gives `Some (prepareRow r reading)`. Every other
    `Ok` and every `Error` gives `None`. `isExcluded` and the `design` / `rhs` assembly are unchanged;
  - the doc's projection sentence is replaced. It now says, in three lines, that each label is read once and the kept
    row is projected by `prepareRow row reading` with the reading that kept it (spec 0050 S10.3). The history aside
    "(the caller — slice 011 — closes …)" is gone; the doc describes the contract.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`:
  - `buildDesign keeps only the requested kind, …`: the stub projection takes an ignored `(_ : NameReading)` after its
    row. Its rows, assertions and numbers are unchanged;
  - `section 7.2 end-to-end: …`: `prepareRow` takes `(reading : NameReading)` after its row, and `phi` is
    `reading.objectPhi`. Its `standardReading r.experiment` call is deleted, and with it the failure on that read's
    `Error`. One comment line says why. The `families` annotation follows the new projection type.

## Decisions

- **`standardReading` stays.** It is no longer called by any projection, but the naming-template fact
  (`the standard naming template reads CPL-(QZ#90-LR#90)-CPL …`, line 437) still uses it to read a literal label. It
  is not a projection, and the brief deletes only the 7.2 projection's read.
- **One `List.choose`, no `(row, reading)` pair.** The kept rows, their order and their readings are what the
  `List.filter` kept before. The predicates are pure, so evaluating them in the guard rather than the filter does not
  change the outcome. Projecting inside the `choose` avoids an intermediate two-tuple list.
- **The match lists `Ok _ | Error _ -> None` explicitly**, not `_ -> None`. The two ways to drop a row stay visible:
  a reading of another kind or row, and a label the template does not read.

## Testing state

Development runs of each gate's own command from `0350.gates`, over the final tree. The gate engine runs the gates
themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error` in stdout; warnings only NU1902 ×12 (exempt advisory) and the vendored MathNet SYSLIB0051 ×4 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1506 (unchanged from 0349, ELEV004 excluded) | 217 s | 600 s |
| constructor-unit-tests | Passed!, 1555 of 1555, 0 failed, 0 skipped (count unchanged from 0349) | 70 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed, 0 skipped (count unchanged) | 3 s | 7200 s |
| invariant-s35-\* (20) | each decoded `git grep` found nothing (exit 1, empty), both as written and with `--untracked` | under 1 s each | 300 s |

- The unit-tests and constructor-unit-tests runs overlapped in time. Both stayed inside their deadlines.
- **Numbers are bit-identical by construction.** The old 7.2 projection computed φ as
  `(standardReading r.experiment).objectPhi`, that is `NamingTemplate.standard.readName r.experiment.nameText`.
  `buildDesign` now makes that same pure call on the same `nameText` and passes its `Ok` reading on. Every kept row
  therefore gets the same φ, and every design row and target holds the same doubles. The stub fact never read the
  reading. The class still passes 25 of 25, with the 7.2 fact inside `MuellerReference.dataMatrixTol`.
- LF only: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat` for both files.

## Artifacts

- `.artifacts/0350-build.log`
- `.artifacts/0350-elevation.log`
- `.artifacts/0350-unit-tests.log`
- `.artifacts/0350-constructor-unit-tests.log`
