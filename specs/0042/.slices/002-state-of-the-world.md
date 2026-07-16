# State of the world — spec 0042, slice 002 (IMPLEMENT) — attempt 02

## Where we are

Slice 002 is the second slice of arc 0042. It extends the `MuellerReconstruction`
domain module (seeded by slice 001) with the pure column-major linear-algebra
core the least-squares Mueller reconstruction will build on: the Kronecker design
row, the order-F vec / un-vec pair, and a Frobenius difference readout — all
expressed through the EXISTING `Propagation` seams (`muellerOfRows` /
`muellerElement`), no new 4×4 algebra. Attempt 01 delivered all of that but
failed the `build` gate on a single F# structural error in the *test* file
(`FS0960`: a `let` fixture placed after the `[<Fact>]` members in a class type).
This attempt fixes that ordering; the production code is unchanged.

## What's working

- Fix the attempt-01 build failure: move the `asymmetricM` test fixture ahead of
  the `[<Fact>]` members so all `let` bindings precede members (F# FS0960).
- Keep the column-major core intact: `kron4`, `vecColumnMajor`,
  `muellerOfVecColumnMajor`, and the `MatrixDiff` record + `frobeniusDiff`.
- Keep the 4 BerremanTests facts: the vec round-trip, the column-major transpose
  guard (`kron4 s a · vecColumnMajor M = a·(M·s)`), and two `frobeniusDiff` facts.
- Read/write every matrix element only through the `Propagation` seams; no direct
  `RealMatrix4x4` indexing.

## Tests

Gate roster for this slice (from `002.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the `IMPLEMENT` worker's **Invariant 6 (act only; run no checks)**, this
worker did NOT execute the gates — the arc-runner gate engine runs them after
exit. Coverage in `BerremanTests/MuellerReconstructionTests.fs` (unchanged
bodies, fixture reordered):

- **vec round-trip** — `muellerOfVecColumnMajor (vecColumnMajor M) = M` over a
  non-symmetric M;
- **column-major transpose guard** — `dot(kron4 s a, vecColumnMajor M) = a·(M·s)`
  within `allowedDiff` for chosen `s, a` and the non-symmetric M (a row-major
  flatten would break it, pinning order F);
- `frobeniusDiff` self-diff is zero; a single perturbed element is localized
  exactly (`argMax`, `maxAbs`, `frobenius`, `meanAbs`).

The transpose-guard and round-trip identities were re-derived by hand this round
and hold; comparisons reuse the shared `allowedDiff`.

```yaml
gates:
  berreman_unit_tests:  0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the
worker authors — 0 is the safe floor here.)

## Architecture

- **Class-type binding order.** In an F# class type (`type … () =`) all `let`
  bindings must precede the first member (FS0960). The two shared fixtures
  (`assertMuellerEqual`, `asymmetricM`) therefore sit together at the top of the
  type, ahead of the `[<Fact>]` members — the correct, minimal fix rather than a
  module-`let` conversion.
- **One convention, pinned by a test.** The vec order (numpy order F,
  column-major: `k = 4*i + j` carries `M[j, i]`) is stated once; the transpose
  guard is exactly the test that fails if the mapping is ever flipped to
  row-major. The round-trip alone would not catch a consistent row-major pair, so
  both facts are needed and independent.
- **Seam-only element access.** No primitive touches `RealMatrix4x4` directly:
  reads go through `Propagation.muellerElement`, writes through
  `Propagation.muellerOfRows` — the engine's backing-matrix type stays a single
  IO seam.
- **`MatrixDiff` is elevated** — a named record with an `(i, j)` argmax, not a
  bare float tuple, per the repo's "elevate every primitive" discipline.

## Deferred

- Assembling the full design matrix from many measurements and solving the
  least-squares system for `M` (and any regularization / weighting) is left for
  later slices — this slice provides only the per-measurement building blocks.
- `frobeniusDiff` has no production caller yet; it is the reconstruction residual
  metric a later slice's fit/report step will consume. Covered by unit tests now.

## Gotchas

- **FS0960 in F# class types.** A `let`/`do` binding cannot follow a `member` in a
  `type … () =` class. When adding a shared fixture to an xUnit test class, put it
  at the TOP with the other `let`s, never inline among the `[<Fact>]` members.
  This was the sole attempt-01 build failure.
- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`, which does not
  exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md`. Read the
  latter; do not block on the literal task-file path.
- **Invariant 6 vs. base step 5.** The base "run gates locally" step is
  superseded by the `IMPLEMENT` delta's Invariant 6; this worker ran no
  build/test. Hence the 0 baseline in the `gates:` block.
- **Line-ending check.** `od -c | grep '\r'` gives a false positive in this Git
  Bash; verify LF with `od -tx1 | grep -o '0d' | wc -l` (→ 0). Both edited files
  are pure LF.
- **Index arithmetic.** `k = 4*i + j` with `i = k/4`, `j = k%4`; the un-vec reads
  row `r` / col `c` as `v[4*c + r]`. Getting this backwards is precisely what the
  transpose guard catches.

## Changelog

- 2026-07-15 — slice 002 (IMPLEMENT) attempt 02: fix the attempt-01 `build`
  failure (FS0960 — the `asymmetricM` test fixture sat after the `[<Fact>]`
  members in a class type) by moving the fixture ahead of the members. Production
  code (`kron4`, `vecColumnMajor`, `muellerOfVecColumnMajor`, `MatrixDiff` +
  `frobeniusDiff`) and all test bodies unchanged.
- 2026-07-15 — slice 002 (IMPLEMENT) attempt 01: add the column-major
  linear-algebra core to `MuellerReconstruction`; add 4 BerremanTests facts (vec
  round-trip + column-major transpose guard + two `frobeniusDiff` facts).
