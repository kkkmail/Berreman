# 007 — Impl plan (retry attempt 02): cone-fan averaging fix

## Scope of this re-spawn

The code-judge routed slice 007 back on a single confirmed correctness gap
(AC-E6), with an explicit retry hint. This round is a tightly-scoped fix, NOT a
re-implementation. Everything else in the slice already shipped green
(R-1..R-10, AC-E1..E5/E7..E10) and is left untouched.

## The defect

`SourceSpec.expand` (`SourceSpec.fs`) fans a `ConeAcceptance` of N samples into N
sub-angles, but the no-Gaussian branch of `beamInfos` assigned every angle weight
`1.0`. `SourceCombination.combine` is a pure non-normalising weighted sum folded
from `StokesVector.Zero`, so an N-sample cone source produced N× the
Stokes/intensity of a collimated source of equal `intensity` — it SUMMED where
AC-E6 requires the combined cone result be AVERAGED, and it skewed the AC-E8
multi-source weighted incoherent sum by the same factor. The two sibling
expansions already normalise (Gaussian → sum 1; unpolarized → 0.5/0.5); the cone
path was the lone inconsistency.

## Changes

1. `SourceSpec.fs` `expand` — the `None` (no-Gaussian) `beamInfos` branch now
   weights each of the N cone angles by `1.0 / N` (`coneWeight`). Collimated
   (N = 1) reduces to unit weight, unchanged. The downstream unpolarized 0.5/0.5
   split and the per-source `intensity` multiplier compose on top, so the
   per-source weights now sum to `intensity` as intended.
2. `SourceSpec.fs` `SpectralProfile.sample` — guard `numberOfPoints = 0` with
   `max 1 r.numberOfPoints` (the reuse critic's optional flag), matching the
   cone's `max 1 cone.samples` precedent. Avoids a `0/0` NaN on a degenerate range.
3. `SourceExpansionTests.fs` AC-E6 cone test — add the missing weight assertions:
   the collimated sample carries unit weight; the 7-sample cone carries weight 1/7
   per entry and the per-sample weights sum to 1. This exercises the per-sample
   weights the gate was previously blind to.

## Explicitly left as-is (per retry hint)

- reuse F1/F2/F3 duplications (`MaterialImport` CSV skeleton, range sampling,
  hand-indexed `scaleStokes`) — spec-compliant follow-ups.
- the inert `Coherent`/`Incoherent` flag — path selection is deferred to the
  substrate / Part F by the spec.

## Risk

Low. Three localized edits, no signature or type changes. The fix is mechanical
and matches two existing normalised siblings. Gates: build, unit-tests,
constructor-unit-tests.
