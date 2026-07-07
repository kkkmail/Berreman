# Impl log — spec 0033, slice 020 (IMPLEMENT)

## Progress

- [x] Read task file, worker system prompt (per-anchor IMPLEMENT delta + shared
  base), project prompt, slice spec 020, slice 019 state-of-the-world.
- [x] Scouted all touch points: Geometry.fs (RotationConvention :578,
  Rotation :601/create :604, rotatePiX :616, rotateHalfPiY :622), Media.fs
  (Layer.rotate :30), MaterialProperties.fs (OpticalProperties.rotate :187),
  ElementId.fs (SampleLayer + all literals), Propagation.fs (ResolvedSample /
  sampleToSystem — its doc carried the "step 20 adds it" marker), every
  SampleLayer construction site (grep `materialId =`), Storage (no
  SampleLayer contact), TestWindows (reads structure only, builds none).
- [x] Impl plan written.
- [x] ElementId.fs: CrystalOrientation (+ toRotation) + SampleLayer.orientation
  + the 8 in-file literals defaulted to PrimaryAxes.
- [x] Propagation.fs: ResolvedLayer carries the orientation; applied at build
  time in the step-1 system builder path (ResolvedLayer.getLayer →
  Layer.rotate for non-identity orientations).
- [x] Tests: 4 new orientation tests in PropagationTests; 3 + 5 existing
  literals updated in PropagationTests / LibraryProxyTests.
- [x] Advisory local gate runs — all five green on the first run.
- [x] LF check (`git diff --numstat` = `--ignore-cr-at-eol`, no CRLF churn).
- [x] State-of-the-world.

## Files modified

- `OpticalConstructor.Domain/ElementId.fs` — `open Berreman.Geometry`; NEW
  `CrystalOrientation = PrimaryAxes | EulerRotation of convention :
  RotationConvention * phi : Angle * theta : Angle * psi : Angle` with
  `member toRotation : Rotation` (PrimaryAxes ⇒ `RealMatrix3x3.identity |>
  Rotation`; EulerRotation ⇒ `Rotation.create convention phi theta psi |>
  Rotation`, Geometry.fs:604), placed right before `SampleLayer`;
  `SampleLayer` gains `orientation : CrystalOrientation`; the 8 construction
  literals (`filmStructure`, `plateStructure`, the quarter-wave + EUV cells,
  the trailing QW glass layer, the langasite film) spell the
  `orientation = PrimaryAxes` default.
- `OpticalConstructor.Domain/Propagation.fs` — NEW `ResolvedLayer`
  (`layerWithDisp : LayerWithDisp` + `orientation : CrystalOrientation`)
  whose `getLayer w` builds the engine `Layer` and, for a NON-identity
  orientation, rotates it via `Layer.rotate` (Media.fs:30 →
  `OpticalProperties.rotate`, MaterialProperties.fs:187); `PrimaryAxes`
  returns the layer untouched (tensors exactly as stored).
  `ResolvedSample.films/substrate` re-typed to `ResolvedLayer`;
  `resolveLayer` threads `l.orientation` through; `sampleToSystem`'s body
  text is unchanged (`f.getLayer w` now dispatches through `ResolvedLayer`)
  and its "(no rotation yet — step 20 adds it)" doc replaced with the
  orientation-application note.
- `OpticalConstructor.Tests/PropagationTests.fs` — 3 existing literals gain
  `orientation = PrimaryAxes`; NEW "Spec 0033 (020) — crystal orientation"
  section (+4 tests, see Testing state).
- `OpticalConstructor.Tests/LibraryProxyTests.fs` — 5 existing literals gain
  `orientation = PrimaryAxes` (mechanical consequence of the new record
  field; no test bodies changed).

## Decisions

- **`CrystalOrientation` lives in `Library` (ElementId.fs), directly before
  `SampleLayer`** — the type is sample-structure DATA (the slice's "nothing
  is stored rotated"), and `SampleLayer` is its only holder; no new file or
  compile-order change needed.
- **The orientation rides `ResolvedLayer`, not a rotated tensor.** The
  resolve step (`resolveSampleMaterials`) cannot rotate: it stores
  `OpticalPropertiesWithDisp` (functions of λ), and rotating there would
  either store rotated data or wrap the dispersion seam. So resolution
  carries the orientation as data and the STEP-1 SYSTEM BUILDER applies it —
  `sampleToSystem` → `ResolvedLayer.getLayer` → `Layer.rotate` — exactly the
  slice's stated seam (worked precedent:
  `Analytics/Examples/ActiveCrystalComparison.fsx:136`).
- **`PrimaryAxes` skips the rotate call entirely** (match, not an
  identity-rotate), so unrotated tensors are bit-identical to the stored
  ones — proven by the untouched legacy-equality acceptance test and the new
  PrimaryAxes test.
- **The substrate plate rotates too**: the substrate is a `SampleLayer`, so
  it carries (and honours) its own orientation through the same path — the
  uniform interpretation of "give SampleLayer an orientation field"
  (recorded here because the slice's examples mention only film layers).
- The F# record has no implicit default; "defaulting to PrimaryAxes" is
  spelled explicitly at every construction site (16 literals across 4
  files), keeping construction sites total and compiler-checked.

## Testing state

All five roster gates pass in local ADVISORY runs (the arc-runner gate engine
re-runs them authoritatively after exit):

- `build` — exit 0, 0 errors, 94 warnings (the pre-existing MSB3277/FS1125
  noise, same count as slices 018/019; none from the edited files).
- `unit-tests` — 119 passed, 5 skipped (pre-existing), 0 failed (= baseline;
  no core file touched).
- `constructor-unit-tests` — 377 passed, 0 failed (373 baseline + 4 new
  orientation tests).
- `ui-smoke` — 59 passed, 0 failed (= baseline).
- `ui-tests` — 263 passed, 0 failed (= baseline).

The 4 new tests (all in PropagationTests):

1. `an EulerRotation layer's built system equals rotating the same layer
   directly` — uniaxial film at Euler (30°, 40°, 50°) under `ZmXpZm`; the
   built films equal `Layer.rotate` applied to the PrimaryAxes-built films,
   AND differ from the unrotated films (the rotation is non-trivial).
2. `PrimaryAxes builds unrotated tensors` — the built film equals
   `{ properties = OpticalProperties.uniaxialCrystal; thickness = 1000 nm }`
   exactly.
3. `an EulerRotation spelling rotatePiX's angles equals the named shortcut
   applied directly` — EulerRotation (ZmXpZm, 0, π, 0) ≡ `Layer.rotatePiX`
   (the named shortcuts remain available for tests).
4. `an EulerRotation substrate plate rotates the substrate tensors the same
   way` — the substrate-plate path through the same seam.

## Artifacts

- `specs/0033/.artifacts/020-build.log`
- `specs/0033/.artifacts/020-unit-tests.log`
- `specs/0033/.artifacts/020-constructor-unit-tests.log`
- `specs/0033/.artifacts/020-ui-smoke.log`
- `specs/0033/.artifacts/020-ui-tests.log`

## Gotchas

- **`Rotation.create` returns a bare `RealMatrix3x3`, not a `Rotation`**
  (Geometry.fs:604-608) — every named shortcut wraps it with `|> Rotation`,
  and `CrystalOrientation.toRotation` must do the same. Easy to miss because
  the member LOOKS like a case constructor.
- The new-test equalities are EXACT (`Assert.Equal<Layer list>`), which is
  sound because both sides run the identical float pipeline —
  `Rotation.create` from the same `Angle`s, then the same
  `OpticalProperties.rotate` — and it matches the file's existing
  legacy-equality precedent (`Assert.Equal<OpticalSystem>`).
- `OpticalConstructor.Tests`' `LibraryProxyTests.fs` is touched only because
  adding a record field breaks record literals; `orientation = PrimaryAxes`
  added to 5 literals, zero behavioural change (the project IS in this
  slice's `touches`).
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–019); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–019.
