# Code judge -- 020.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\020.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\020-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\020-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none this cycle)

## Rationale

All five roster gates pass, no critic filed a critique this cycle, and a
direct read of `git diff HEAD` confirms the diff does exactly what the slice
spec ordered and what the worker's SoW and impl-log claim.

**Slice requirements are met, verified against the code.**
`CrystalOrientation = PrimaryAxes | EulerRotation of convention :
RotationConvention * phi : Angle * theta : Angle * psi : Angle` is added to
`OpticalConstructor.Domain` (ElementId.fs, `Library` module, directly before
`SampleLayer`), with `member toRotation : Rotation` — `PrimaryAxes` maps to
`RealMatrix3x3.identity |> Rotation`, `EulerRotation` to `Rotation.create
convention phi theta psi |> Rotation` (correctly wrapping the bare
`RealMatrix3x3` that `Rotation.create` returns, the exact pitfall the
impl-log's Gotchas records). `SampleLayer` gains `orientation :
CrystalOrientation`; since F# records have no implicit defaults, "defaulting
to PrimaryAxes" is spelled explicitly at every construction site — the diff
shows 8 literals in ElementId.fs, 5 in LibraryProxyTests.fs, 3 in
PropagationTests.fs, matching the claimed 16. The rotation is applied at the
one build seam the spec named: the new `ResolvedLayer` (LayerWithDisp +
orientation) replaces bare `LayerWithDisp` in `ResolvedSample`, and its
`getLayer` rotates a non-identity orientation via `Layer.rotate` →
`OpticalProperties.rotate`; `sampleToSystem` routes both `films` and the
substrate plate through `getLayer`, so nothing is ever stored rotated and the
substrate honours its orientation through the identical path. The stale
"(no rotation yet — step 20 adds it)" doc marker is replaced.

**Acceptance criteria are directly asserted by tests in the diff.** The two
mandated tests exist verbatim in PropagationTests: "an EulerRotation layer's
built system equals rotating the same layer directly" (uniaxial film at Euler
(30°, 40°, 50°) under ZmXpZm, equal to `Layer.rotate` on the
PrimaryAxes-built films, plus a non-triviality assertion that the rotated
tensors differ from the stored ones) and "PrimaryAxes builds unrotated
tensors" (exact equality with the engine's stored uniaxial properties). Two
further tests strengthen coverage: the EulerRotation(ZmXpZm, 0, π, 0) ≡
`rotatePiX` named-shortcut equivalence the spec called out, and the
substrate-plate rotation path. Constructor tests went 373 → 377; every other
suite sits at its baseline, consistent with a change confined to the slice's
declared touches.

**New public surface is exercised.** `CrystalOrientation`/`toRotation`,
`SampleLayer.orientation`, and `ResolvedLayer.getLayer` are all driven by the
four new tests. The only untested branch is `PrimaryAxes.toRotation` itself
(dispatch in `getLayer` deliberately skips the identity rotation so unrotated
tensors stay bit-identical — a design choice I consider sound); the
observable PrimaryAxes behavior is directly asserted, so this is a nit, not a
gap. The exact `Assert.Equal<Layer list>` equalities are justified — both
sides run the identical float pipeline — and match the file's existing
legacy-equality precedent.

**SoW and impl-log line up with the diff** in every checked particular
(files touched, literal counts, seam location, test descriptions, gate
counts). The Deferred section honestly scopes out UI wiring, Storage
serialization of the new field, and wedge substrates as later slices, which
matches the spec's silence on those fronts. Nothing here needs a re-spawn;
the slice is done on cycle 1.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass, no critic critiques were filed, and git diff confirms the slice contract is met: CrystalOrientation (PrimaryAxes | EulerRotation of RotationConvention * phi * theta * psi) with toRotation : Rotation added to OpticalConstructor.Domain; SampleLayer.orientation defaulting to PrimaryAxes at all 16 construction sites; rotation applied only at the step-1 system-builder seam via ResolvedLayer.getLayer -> Layer.rotate -> OpticalProperties.rotate for films and substrate alike, nothing stored rotated. Both acceptance tests (EulerRotation build equals direct Layer.rotate; PrimaryAxes builds exactly the stored tensors) are present in PropagationTests plus rotatePiX-shortcut and substrate-plate tests (constructor tests 373 -> 377, all other suites at baseline). SoW and impl-log match the diff in every checked detail.", "retry_hint": ""}
```
