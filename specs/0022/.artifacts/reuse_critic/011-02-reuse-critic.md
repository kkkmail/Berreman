# Reuse critique -- 011.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (the `OpticalConstructor.Optimization`
  and `.Ui` production files, both test hosts, and the slice-009/010 helpers the
  attempt-02 diff builds on).
- Files inspected: 12/200 (the 4 slice files + `SynthesisFitPageTests.fs`, the two
  attempt-02 edits `OptimizationInterface.fs`/`AlglibAdapter.fs`, and the sibling
  helpers/fixtures cited below: `LocalRefinementTests.fs`, `GradientDiscretizeTests.fs`,
  `Media.fs`).
- Extensions: the configured bounds (`.py,.md,.json`) do NOT match this F# project;
  the diff is entirely `.fs`, so I walked `.fs` directly — the authoritative evidence
  base here. No cap was tripped.

## Resolved since cycle 1

**F1 (triplicated SSR fold `sumSq`) is fixed.** Attempt-02 lifted the Σ residualᵢ²
definition into one `let internal sumSq` at `OptimizationInterface.fs:26`, deleted the
private copy from `AlglibAdapter.fs` (the byte that was at `:23` is now a pointer
comment at `:22`), and routed `FitQuality.reportFrom` (`FitQuality.fs:119`),
`Synthesis.merit`/`systemMerit` (`Synthesis.fs:38`/`:44`), and the adapter's scalar
objective (`AlglibAdapter.fs:63/107/112/168/171`) all through that single helper. The
load-bearing χ²/merit definition can no longer drift between the optimizer, the
synthesis loops, and the fit report. This was the only cycle-1 finding with real
correctness-drift potential and it is genuinely closed. The follow-on `merit = sumSq ∘
residual` one-liner at `Synthesis.fs:38` is no longer a duplication concern — it is a
trivial composition over the now-shared fold, not a re-derivation.

## Findings

### F2: `thicknessMeters` thickness-unwrap still duplicated within the slice

- **Worker added:** `thicknessMeters` at `Synthesis.fs:56`
  (`match l.thickness with Thickness t -> t / 1.0<meter> | Infinity -> fallback`) and a
  near-identical `thicknessMeters` at `OptimizationTests.fs:70`
  (same unwrap, but `| Infinity -> infinity`).
- **Existing helper:** the same `Thickness t -> t / 1.0<meter>` unwrap already recurs at
  `LocalRefinementTests.fs` and at `GradientDiscretizeTests.fs:19`
  (`let private thicknessMeters (l : Layer) : float<meter>`). The `Thickness` DU itself is
  `Media.fs:12-13`; it exposes constructors (`Thickness.nm`/`.mkm`/`.mm`, `Media.fs:16-18`)
  but **no** unwrap/`inMeters` accessor, so every call site re-rolls the `match`.
- **Why it matters:** the two slice copies differ only in their `Infinity` arm (a finite
  `fallback` vs `infinity`) — exactly the subtle divergence that bites later, since a
  caller that picks the wrong file silently gets a different out-of-band value for an
  infinite substrate. The production `Synthesis.thicknessMeters` is `private`, so the test
  genuinely cannot call it as-is; that is an argument for a single shared accessor, not for
  a third hand-rolled `match`.
- **Suggested action:** advisory only, and lower-confidence — the cleanest fix (a one-line
  `Thickness.inMeters` accessor on the DU in `Media.fs`, called from both production and
  test sites) is *designing a helper that does not yet exist*, which is outside the strict
  reuse rubric and which the cycle-1 code judge already ruled out of this slice's scope
  under §0 #6. At minimum, the test should not re-declare what `Synthesis` already defines.
  The state-of-the-world already carries this as a Deferred item; I concur it is hygiene,
  not a blocker.

### F3: Test-fixture helpers still duplicated instead of shared

- **Worker added:** `glass`, `mkSystem`, `reflectanceOf` at
  `SynthesisFitPageTests.fs:30/32/41`, and `glass`, `mkFilmSystem`, `diagonalLight`,
  `reflectanceOf`, `ssr` at `OptimizationTests.fs:44/48/59/62/68`.
- **Existing helper:** the byte-for-byte equivalents already exist in
  `LocalRefinementTests.fs:29` (`glass`, same `RefractionIndex.create 1.52`), `:34`
  (`mkSystem`), `:55` (`diagonalLight`), `:58` (`reflectanceOf`), `:61` (`ssr`) — and
  `SynthesisFitPageTests.fs` lives in the SAME project/namespace
  (`OpticalConstructor.Tests`) as `LocalRefinementTests.fs`, so the existing fixtures are
  directly reachable.
- **Why it matters:** within `OpticalConstructor.Tests`, `SynthesisFitPageTests` and
  `LocalRefinementTests` now carry three identical private fixtures. `LocalRefinementTests`
  documents *why* `diagonalLight` is 45° (the s-pol ρ≡0 trap) at `:50-54`; the
  `OptimizationTests` copy keeps a shortened rationale at `:59-61`, but a future editor
  "fixing" one fixture will not see the siblings. The `OptimizationTests.fs` copy is the
  weaker half of this finding: it sits in the separate `BerremanTests` project, where
  sharing would need a cross-project test-support assembly that §0 #6 may not justify.
- **Suggested action:** advisory only — extract the shared in-project fixtures
  (`glass`/`mkSystem`/`reflectanceOf`/`diagonalLight`/`ssr`) into one module under
  `OpticalConstructor.Tests` and have both `LocalRefinementTests` and
  `SynthesisFitPageTests` open it; treat the cross-project `OptimizationTests` clone as
  leave-as-is unless a shared test-support project already exists. Note the many other
  `let private glass` declarations across `BeamRoutingTests`/`CurvedElementsTests`/
  `StackEditTests`/`SourceExpansionTests` use `RefractionIndex 1.5` and predate this slice
  — they are not the worker's additions and are out of scope here. The state-of-the-world
  already records this as a maintenance-hygiene Deferred item for a later slice.

## Bottom line

The attempt-02 diff closes the one reuse finding that carried real correctness risk:
the SSR/χ²/merit fold is now defined exactly once and called from all three Optimization
modules, so the optimizer, synthesis loops, and fit report cannot disagree about what
they minimise. The remaining F2/F3 are the same low-severity convenience-helper and
test-fixture duplication flagged in cycle 1, and the cycle-1 code judge already routed
them to `Deferred` (F2 needs a new `Media.fs` accessor that is outside the pure-reuse
rubric and §0 #6 scope; F3's strongest half is the in-project `SynthesisFitPageTests`
↔ `LocalRefinementTests` fixture overlap). My read: nothing here is substantive enough
to compel a re-spawn — the load-bearing seams (`LocalRefinement.refine`/`refineWith`,
`MeritFunction.buildResidual`, `DesignParameters`, the `MathNetNumericsMath` `RealMatrix`
seam, the `plotComparison` payload, the closed `TerminationReason` DU, and now the shared
`sumSq`) are all reused rather than forked. The judge can reasonably ship and leave F2/F3
as the standing Deferred notes.
