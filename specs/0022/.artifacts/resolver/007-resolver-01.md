# Resolver decision -- 007.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\007.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\007-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\007-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0022\.artifacts\code_judge\007-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\007-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\007-02-reuse-critic.md`
- Direct read: `OpticalConstructor.Domain/SourceSpec.fs:262-309` (the `expand` function) to confirm the high-signal finding first-hand.
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category `failures-cap`).

## Diagnosis

The cause is concrete, isolated, and I confirmed it by reading the code rather than trusting the artefacts. `SourceSpec.expand` (`SourceSpec.fs:285-294`) builds its `beamInfos` in two arms. The `None` (cone-only) arm applies `let coneWeight = 1.0 / float (List.length angleInfos)` and maps every cone angle to that weight (line 293), so an N-sample cone averages to total weight 1 — this is the cycle-01 retry fix and it is genuinely correct. The `Some beam` (cone + Gaussian) arm at lines 287-291 instead does `angleInfos |> List.collect (fun info -> GaussianBeamSpec.angularSpectrum beam info.waveLength |> List.map (...))`. Each of the N cone angles spawns a Gaussian fan whose weights already sum to 1, and the arm never multiplies by the `1/N` cone-averaging factor. So a source with `cone = Some {samples = N}` AND `gaussianBeam = Some` contributes total weight N instead of 1.

This lands squarely in the slice's own load-bearing contract. `SourceCombination.combine` is a non-normalising weighted incoherent `StokesVector` sum, so per-source weights must already average; an N× over-weighted source silently distorts the AC-E8 multi-source result and violates AC-E6 ("cone result MUST be averaged"). It is the exact over-weighting class the cycle-01 retry fixed for the cone-only branch (impl-log "retry 02"), left unrepaired in the combined branch. No test sets both `cone` and `gaussianBeam`, so all three gates stay blind to it — the defect survives a green board.

The code-judge (cycle 2) diagnosed this precisely, chose `route-back-to-worker`, and the only reason this reached me is that the per-launch failure budget (2/2) was exhausted before that route-back could run. This is not the "worker oscillating between fixes" pattern the failures-cap category usually signals: cycle 01's cone-only fix held, and the combined branch is a distinct, never-yet-touched instance of the same one-line pattern. One more targeted attempt plausibly closes the slice.

The fix is bounded: in the `Some beam` arm, scale each Gaussian-fan weight by `1.0 / float (List.length angleInfos)` so the combined source totals weight 1, mirroring the `None` arm and the unpolarized 0.5/0.5 split; then add one expansion test setting both `cone` and `gaussianBeam` and asserting the expanded weights sum to the source `intensity`. That touches exactly two files (`SourceSpec.fs`, `SourceExpansionTests.fs`), introduces no new design, dependency, or scope, and needs no operator judgement. The remaining critic findings — reuse F1-F4 (CSV-skeleton and range-loop near-duplications, the `scaleStokes` splat, the `[<Measure>] K` locus) and the inert-`Incoherent`-flag / deferred-`view` notes — are explicitly advisory follow-ups that do not touch shipped-behaviour correctness; both critics and the judge agree they may be routed later. They must NOT be folded in, to keep this final attempt scoped. Confidence is high: the cause is read-confirmed, the fix is one line plus one test, and it stays within all stated caps.

## Verdict

issue-hint

## Hint

In `SourceSpec.expand` (SourceSpec.fs:287-291), the `Some beam` arm collects a unit-sum Gaussian fan over each of the N cone angles without the `1.0 / float (List.length angleInfos)` factor the `None` arm applies on line 293, so a source with both `cone={samples=N}` and `gaussianBeam=Some` sums to weight N instead of 1. Multiply each Gaussian-fan weight by that same per-cone-angle factor so the combined source totals weight 1 (i.e. the source `intensity`), matching the cone-only and unpolarized 0.5/0.5 normalisations. Add one test in SourceExpansionTests.fs that sets BOTH `cone` and `gaussianBeam` and asserts the expanded per-source weights sum to the source `intensity`, not N. Edit only those two files. Leave the reuse-critic findings F1-F4 and the inert-`Incoherent`-flag note as documented follow-ups -- do not broaden scope.

```json
{"verdict": "issue-hint", "rationale": "Read-confirmed concrete defect at SourceSpec.fs:287-291: the cone+Gaussian `Some beam` arm of expand collects a unit-sum Gaussian fan over each of N cone angles without the 1/N cone-averaging factor the `None` arm applies at line 293, so a source with both cone={samples=N} and gaussianBeam=Some sums to total weight N instead of 1, breaking the load-bearing AC-E6 (cone MUST be averaged) and AC-E8 (weighted incoherent sum) contract; no test sets both, so all three green gates are blind to it. This is the same over-weighting class the cycle-01 retry fixed for the cone-only branch, left unrepaired in the combined branch -- not oscillation but a distinct untouched instance, so one more targeted attempt plausibly closes the slice. The fix is one line (scale the Gaussian weights by 1/N) plus one covering test, touching exactly two files (SourceSpec.fs, SourceExpansionTests.fs), with no new design, dependency, or scope; the reuse F1-F4 and inert-Incoherent-flag findings are advisory follow-ups all parties agree may be routed later. Within the 5-sentence and 2-file caps at high confidence.", "operator_reply_text": "In `SourceSpec.expand` (SourceSpec.fs:287-291), the `Some beam` arm collects a unit-sum Gaussian fan over each of the N cone angles without the `1.0 / float (List.length angleInfos)` factor the `None` arm applies on line 293, so a source with both `cone={samples=N}` and `gaussianBeam=Some` sums to weight N instead of 1. Multiply each Gaussian-fan weight by that same per-cone-angle factor so the combined source totals weight 1 (i.e. the source `intensity`), matching the cone-only and unpolarized 0.5/0.5 normalisations. Add one test in SourceExpansionTests.fs that sets BOTH `cone` and `gaussianBeam` and asserts the expanded per-source weights sum to the source `intensity`, not N. Edit only those two files. Leave the reuse-critic findings F1-F4 and the inert-`Incoherent`-flag note as documented follow-ups -- do not broaden scope.", "confidence": "high"}
```
