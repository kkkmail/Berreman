# Code judge -- 007.slice-md cycle 3

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\007.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\007-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\007-impl-log.md`
- Gate results: build = pass, unit-tests = pass, constructor-unit-tests = pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\007-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\007-02-reuse-critic.md`
- Retry budget: worker_review_max_cycles = 3, current cycle = 3, cycles_remaining = 0.

## Rationale

This is the final cycle (`cycles_remaining == 0`), so the verdict is either
`done-green` or `escalate-to-human`; `route-back-to-worker` is no longer
permitted. The slice clears `done-green` ground, so I ship it.

The defect that justified the cycle-2 hold is genuinely closed. Cycle 2 routed
back on one concrete, untested correctness gap: the `gaussianBeam = Some` arm of
`SourceSpec.expand` laid a unit-sum Gaussian fan over each of the N cone angles
without the `1/N` cone-averaging factor the sibling `None` arm applied, so a
source with both `cone = {samples = N}` and `gaussianBeam = Some` summed to total
weight `N` instead of `1` — an `N×` over-weight in the load-bearing AC-E6 / AC-E8
averaging path. Impl-log "retry 03" records the fix: `coneWeight = 1.0 / float
(List.length angleInfos)` is hoisted above the `match s.gaussianBeam` and
multiplied into each Gaussian-fan weight, so both arms now normalise to the source
`intensity`. The architecture critic read the code at `SourceSpec.fs:281-309` and
confirms the fix is real, and a dedicated AC-E9 test ("a cone + Gaussian source
normalises to the source intensity, not N") was added — constructor-unit-tests
rose 106 → 107, so the previously-blind branch is now covered. The "expand on the
boundary, average on output" contract holds across every fan; no `EmField` is
summed across samples or sources.

Gates and coverage support the verdict. All three gates are green (build 0
errors; unit-tests 70 held; constructor-unit-tests 107). R-1..R-10 map cleanly
onto AC-E1..E10 and the tests assert the load-bearing properties directly: cone
weights sum to 1, cone+Gaussian totals intensity, multi-source combine sums
`StokesVector` via `(+)`/`Zero` with no field-level summation, and
`Coherence.Unpolarized` is a flag rather than a sentinel. Every new pure surface
the slice introduces — `toIncidentLight`, `SpectralProfile.sample`,
`ConeAcceptance.sample`, `expand`, `GaussianBeamSpec.angularSpectrum`, and the
`SourceCombination` reducer — is exercised by a test in the diff
(`SourceProjectionTests.fs`, `SourceExpansionTests.fs`). The UI seams are
Avalonia-free per the slice-005/006 precedent and asserted at the model seam
(AC-E10). The SoW and impl-log line up with the diff, including an honest account
of all three attempts and the `getWaveLengthValue` mis-scale Gotcha.

The remaining critic findings do not block. The architecture critic's two
substantive items are both spec-sanctioned forward risks, not unmet 007
requirements: (a) the split spectral/`expand` seam — `expand` folds in only
`LaserLine` and range-based sampling lives in `SpectralProfile.sample` — is
correct because the R-8 signature `SourceSpec -> (IncidentLightInfo * float) list`
carries no wavelength range, and the critic explicitly calls the under-delivery
"spec-sanctioned," with the reconciliation owned by Part F; and (b) the untested
populated-`sources` round-trip is a slice-003 / Part-F concern — R-8 assigns the
`sourceSpec` `$def` schema/serializer implementation to slice-003 storage core,
and the 007 testing plan names no populated round-trip test, so its absence
violates no stated requirement here. The architecture critic's bottom line is
"I would ship this." The reuse critic's F1 (`sample` re-deriving
`RangedVariable.value`), F2 (`SpectralImport` vs `MaterialImport` CSV skeleton),
F3 (`scaleStokes` vs inner `RealVector` scalar multiply), and F4 (net-new
`[<Measure>] K` locus) are real near-miss duplications / pattern divergences, but
the critic itself concludes none touch the correctness of shipped behaviour and
may be routed as follow-up; binding constraint 2 is satisfied because the worker
*calls* the engine seams (`Units.toWaveLength`, `createGaussian` +
`FourierTransform.fft`, `EmField.stokesVector`, the substrate `Multiple` path)
rather than forking them. The inert `Incoherent` flag is in-scope: R-7 scopes this
part to "ONLY the source-level flag" with the solve wiring owned downstream, so
carrying the flag is correct and the test's modest confidence is acceptable.

No escalation trigger is present: the diff does not conflict with itself, no gate
is missing, and no finding describes a deep architectural problem beyond a single
re-spawn. The slice meets `done-green` ground on its own merits — gates green,
slice requirements met, coverage present, SoW faithful — independent of the cycle
cap. I record F1–F4 and the split-seam / populated-`sources` round-trip as
recommended follow-ups for Part F and slice-003.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "Final cycle (cycles_remaining == 0); slice clears done-green ground. The cone+Gaussian over-weighting defect that drove the cycle-2 hold is genuinely fixed -- SourceSpec.expand now hoists coneWeight = 1/N and multiplies each Gaussian-fan weight by it, so both the Some-beam and None arms normalise to the source intensity (architecture critic confirmed by reading SourceSpec.fs:281-309), and a dedicated AC-E9 test now covers it (constructor-unit-tests 106 -> 107). All three gates pass (build 0 errors, unit-tests 70 held, constructor-unit-tests 107). R-1..R-10 map cleanly onto AC-E1..E10, every new pure surface (toIncidentLight, SpectralProfile.sample, ConeAcceptance.sample, expand, angularSpectrum, SourceCombination) is exercised by a test in the diff, the expand-on-boundary/average-on-output contract holds with no field-level summation, and the SoW/impl-log match the diff. Remaining findings are advisory or spec-deferred, not unmet 007 requirements: the split spectral/expand seam is spec-sanctioned (R-8 signature carries no range; range sampling lives in SpectralProfile.sample, reconciliation owned by Part F); the untested populated-sources round-trip is owned by slice-003 storage core (R-8) and the testing plan names no such test; reuse F1-F4 are near-miss duplications the reuse critic itself routes as follow-up and binding constraint 2 is met by calling the engine seams; the inert Incoherent flag is in-scope per R-7. No escalation trigger: diff is self-consistent, no gate missing, no single-respawn-exceeding architectural problem. F1-F4 and the split-seam/round-trip gaps recorded as Part F / slice-003 follow-ups.", "retry_hint": ""}
```
