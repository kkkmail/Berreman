# Architecture critique -- 006.slice-md cycle 1

## Summary

A clean, well-scoped landing of the Part C curved-element domain. The
three named engine seams (`OpticalSystemSolver`, `IncidentLightInfo.rotateY`,
`EmField.propagate`) are reused unchanged, nothing is forked, the
slice-002 `BeamTree` types are imported rather than redefined, and the
file/module shape matches `BeamTree.fs` exactly. The single finding that
deserves the judge's attention is a literal-vs-implemented gap on the
§C.9 grading opt-in flag (R-9), which the worker resolved sensibly but
did not surface in the SoW. One unit test (AC-C7) is also vacuous.

## Spec fit

- **R-9 grading opt-in flag is absent (most important finding).** R-9
  states grading "MUST be opt-in per element (a flag on the
  `ConstructorElement.CurvedMirror`/`Lens` payload, Part A §A.4)." But
  those cases are nullary (`BeamTree.fs:39,41`) and owned by Part A,
  which this slice is forbidden to redefine. The worker implemented
  opt-in as "the caller simply does not call `gradeCoating`"
  (`CurvedElements.fs` `gradeCoating` doc). That is the most defensible
  reading given the non-redefine constraint — adding a payload flag
  would force a cross-tree edit to the Part-A-owned DU — but it is a
  deviation from the literal directive, and neither the impl-log nor the
  SoW `Gotchas` records that the flag was deliberately dropped. The
  smallest fix is a one-line note in the SoW `Gotchas` documenting the
  forced choice so a later slice (or Part J builder) knows the opt-in
  lives at the call site, not on the element.
- **R-7 delivers no curved-element propagation entry point.** R-7 asks
  the slice to "carry the beam from one curved element's zone output to
  the next element" via `EmField.propagate`. `CurvedElements.fs` adds no
  such function; the docstring says propagation is "reused via
  `BeamTree.childIncidentField`," and AC-C7 calls `outgoing.propagate`
  directly. The "no second propagator" half of R-7 is honoured cleanly,
  but the "carry the beam" half is satisfied only by omission — callers
  must wire `childIncidentField` themselves. Acceptable for a domain-only
  slice, worth one sentence of acknowledgement.
- **`attachCurvedElement` also matches `FlatMirror` (`CurvedElements.fs`,
  the `CurvedMirror | FlatMirror ->` arm).** §C.6 scopes the fan to
  `Lens`/`CurvedMirror` only. Folding `FlatMirror` in is harmless (a
  mirror is reflected-only either way) but is minor scope creep into a
  flat element from a curved-element function; the catch-all arm already
  returns the bare node, so `FlatMirror` could simply fall there.

## Consistency

Strong. The placeholder `module OpticalConstructor.Domain.CurvedElements`
was correctly promoted to `namespace OpticalConstructor.Domain` + nested
`module CurvedElements =`, matching `BeamTree.fs:1,17` precisely. Units
discipline is consistent throughout — every length is `double<meter>`,
the `0.05` grading fraction is documented as dimensionless and applied
only to meter-valued `Thickness.Thickness`, and `Angle` is the engine
`Geometry.Angle` so it feeds `rotateY` directly. The mirror drop reuses
the slice-002 `BeamNode.attach` smart constructor (`BeamTree.fs:74`)
rather than a parallel rule, which is exactly the right seam.

## Risks

- **AC-C7 is a tautological test.** `CurvedElementsTests.fs:212,215`
  binds `let advanced = outgoing.propagate gap` and then asserts
  `Assert.Equal(eNorm (outgoing.propagate gap), eNorm advanced, 12)` —
  i.e. it compares `outgoing.propagate gap` to itself. It proves the
  call is deterministic, not that propagation does anything; the only
  load-bearing assertion is `not (List.isEmpty advanced.emComponents)`.
  AC-C7 is nominally green but functionally unverified. A meaningful
  check would compare the propagated field against the pre-propagation
  field, or against the `BeamTree.childIncidentField` seam the SoW
  claims is the reuse path, asserting they agree.
- **`localShiftAt` introduces a hidden numerical-difference step.** The
  zone tilt is a central finite difference with step
  `h = abs(semiAperture) * 1e-6` (`CurvedElements.fs`, `localShiftAt`).
  For the `Spherical` figure the normal tilt has an exact closed form
  (`asin(r/R)`/`atan` of the analytic slope), so a numerical derivative
  with a hard-coded step is both more code and a latent precision knob
  not asked for by the spec's minimum-implementation rule. Low risk at
  the tested tolerances, but a candidate for an exact form later.

## Evolvability

The `Aspheric` sag evaluates the polynomial in the dimensionless ratio
`r/R` with meter-valued coefficients (`CurvedElements.fs`, `sag`), which
keeps the type `double<meter>` but is a non-standard even-asphere
convention (standard coefficients carry units of length^(1-2i) against
`r^(2i)`). The worker correctly flagged this as deferred in the SoW, so
it will not silently mislead a later slice — but if a directive ever
pins the coefficient convention, the formula will need revisiting and
any persisted coefficients reinterpreted. Containing all of this inside
the Domain project keeps that future change local.

## Bottom line

I would ship this. The architecture is faithful to §A.5 (one admitted
extension, three reused seams, no forked engine), the layering and
consistency are clean, and seven of eight ACs are backed by real tests.
The two substantive items — the undocumented R-9 flag drop and the
vacuous AC-C7 assertion — are both addressable with small, local edits
(an SoW `Gotchas` line and a non-self-referential propagation assertion)
and neither blocks the Part A–D foundation the hand-off claims. The
judge should weigh whether the AC-C7 test gap is acceptable given the
gate counted it as passing coverage. Suggestions only; the verdict is
the judge's.
