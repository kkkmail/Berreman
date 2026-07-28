# Impl-log — spec 0042, slice 007 (IMPLEMENT — per-row Mueller-reconstruction glue)

## Progress

- [done] Read the read-order chain: `implement_worker.system-md` (IMPLEMENT
  delta) → `arc-runner.system-md` (base protocol) → project prompt
  (`arc-runner.user-md`) → the slice spec (`007.slice-md`) → CLAUDE.md.
- [done] Studied the reference `matrix_glue.py` / `cpl_cpl_analyzer.py` in the
  sibling `optics-mueller` repo (the grammar this slice ports) and the existing
  `MuellerReconstruction.fs` / `Propagation.fs` seams.
- [done] Added the Part-D glue to `MuellerReconstruction.fs` (types + tiny pure
  functions), reusing the existing `Propagation` seams + `retarderMueller` /
  `kron4` — no new 4×4 algebra.
- [done] Added the +90/−90 invariance `[<Fact>]` (the acceptance) to
  `MuellerReconstructionTests.fs`, plus two component-wise closeness helpers over
  the shared `allowedDiff`.
- [done] Verified both edited `.fs` files are pure LF (0 CR bytes via `od`).
- [n/a] Gate execution — NOT run (Invariant 6: the worker acts, runs no checks;
  the arc-runner's gate engine runs `build` / `unit-tests` /
  `constructor-unit-tests` after exit).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  - Added `open System.Text.RegularExpressions` and
    `open OpticalConstructor.Domain.Library` (for `PolarizerKind` / `IdealLinear`,
    exactly as `Propagation.fs` opens it).
  - Appended a new "Spec 0042 (007)" section at the end of the module:
    - types `Family`, `MatrixKind`, `ParsedObject`, `ParsedExperiment`, `DialSign`
      (`.value : float`), `SourceModel`, `AnalyzerModel`;
    - `parseExperiment` (verbatim port of `EXPERIMENT_RE` / `OBJECT_RE`), `family`,
      private `objectRotation`, `matrixKind`, `objectPhi`, `sourceBaseStokes`,
      `physicalAngle`, `analyzerBaseRow`, `effectiveSource`, `effectiveAnalyzer`,
      `designRow`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`
  - Added `assertStokesClose` / `assertVec4Close` `let` helpers (ahead of the
    members, FS0960) over `allowedDiff`.
  - Added the `[<Fact>]` `parseExperiment decodes CPL-(QZ#90-LR#90)-CPL and the
    +90 / -90 object rotation is inert (R(2 phi) invariance)`.

## Testing state

`commit_ready: true`. The slice's single enumerated requirement — the Part-D per-row
glue functions + the +90/−90 invariance fact — is fully addressed this round; nothing
deferred to a "round 2". Per Invariant 6 the gates were not run here; the code was
reviewed at the type level for a green `build` (F# resolution, complete matches,
parenthesised unary-minus / negative-literal arguments, `Int32.TryParse` tuple form,
record-label inference, and the engine `*` / indexer operators all check out) and the
new fact is arithmetically exact (the +90/−90 states differ only by ~1e-16 from
`sin(±π)`, far inside `allowedDiff = 1e-5`).

## Artifacts

None this round — pure Domain code plus one unit test; no captured logs, traces, or
screenshots were produced.

## Gotchas

See `007-state-of-the-world.md` `Gotchas` for the full list. Key recorded choices:
`family` / `matrixKind` are declared TOTAL per the slice's named signatures (the
reference raises on unsupported labels; the data only carries LP/CPL + QZ/LR/AIR, so
the total mapping is faithful, with unknown tokens degrading to `Lp` / `Air`); φ is
exposed via an added `objectPhi` companion to `matrixKind` (both `fst`/`snd` of one
private `objectRotation`, mirroring the reference's single `object_rotation_from_parsed`).
The system-prompt path in the task file drifts (real file under
`.../src/ai_strategy_generator/multistep/`) — same drift the 003–006 workers recorded.
