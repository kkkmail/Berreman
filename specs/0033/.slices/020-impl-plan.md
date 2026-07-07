# Impl plan — spec 0033, slice 020 (IMPLEMENT)

## Goal

Give sample layers a declarative crystal orientation and apply it when the
step-1 system builder assembles the engine system — nothing stored rotated:

1. **`CrystalOrientation`** in `OpticalConstructor.Domain.Library`
   (`ElementId.fs`, right before `SampleLayer`):
   `PrimaryAxes | EulerRotation of convention : RotationConvention * phi :
   Angle * theta : Angle * psi : Angle`, with `member toRotation : Rotation`
   — `PrimaryAxes` ⇒ identity (`RealMatrix3x3.identity |> Rotation`),
   `EulerRotation` ⇒ `Rotation.create convention phi theta psi |> Rotation`
   (Geometry.fs:604). The named shortcuts `Rotation.rotatePiX` /
   `rotateHalfPiY` (Geometry.fs:616/622) stay available for tests.
2. **`SampleLayer.orientation : CrystalOrientation`** defaulting to
   `PrimaryAxes` at every existing construction site (F# records carry no
   implicit defaults — the default is spelled explicitly where layers are
   built: `filmStructure` / `plateStructure` / the multilayer + EUV cells +
   langasite film in `ElementId.fs`, and the test literals).
3. **Apply in the step-1 system builder** (`Propagation.fs`, whose
   `sampleToSystem` doc says "no rotation yet — step 20 adds it"): a new
   `ResolvedLayer` record (`layerWithDisp : LayerWithDisp` + `orientation`)
   replaces the bare `LayerWithDisp` in `ResolvedSample.films` / `.substrate`;
   its `getLayer w` builds the engine `Layer` and, for a non-identity
   orientation, rotates it via `Layer.rotate` (Media.fs:30) →
   `OpticalProperties.rotate` (MaterialProperties.fs:187). `PrimaryAxes`
   skips the rotation entirely (tensors exactly as stored). Worked precedent:
   `Analytics/Examples/ActiveCrystalComparison.fsx:136` rotates a plate
   system by `rotateHalfPiY`.

## Files

- `OpticalConstructor.Domain/ElementId.fs` — `open Berreman.Geometry`;
  `CrystalOrientation` + `toRotation`; `SampleLayer.orientation`; the 8
  in-file layer literals gain `orientation = PrimaryAxes`.
- `OpticalConstructor.Domain/Propagation.fs` — `ResolvedLayer` (record +
  `getLayer`), `ResolvedSample.films/substrate` re-typed, `resolveLayer`
  carries the orientation through; `sampleToSystem` body is unchanged text
  (`f.getLayer w` now dispatches through `ResolvedLayer`), docs updated.
- `OpticalConstructor.Tests/PropagationTests.fs` — 3 existing literals gain
  `orientation = PrimaryAxes`; new "Spec 0033 (020)" section (+4 tests):
  EulerRotation film system = rotating the same layer directly (and differs
  from the unrotated film), PrimaryAxes builds unrotated tensors
  (`OpticalProperties.uniaxialCrystal` exactly), an EulerRotation spelling
  rotatePiX's angles = the named shortcut applied via `Layer.rotatePiX`, and
  the substrate-plate path rotates the same way.
- `OpticalConstructor.Tests/LibraryProxyTests.fs` — 5 existing literals gain
  `orientation = PrimaryAxes` (mechanical consequence of the new field).

## Risks

- Adding a record field breaks every construction site — the grep sweep found
  them only in `ElementId.fs` (8), `LibraryProxyTests.fs` (5),
  `PropagationTests.fs` (3); Storage never touches `SampleLayer`, TestWindows
  only reads it.
- Equality in the tests relies on both paths computing the identical float
  ops (`Rotation.create` from the same `Angle`s, then the same
  `OpticalProperties.rotate`), which is the established precedent — the
  existing legacy-equality acceptance already uses exact
  `Assert.Equal<OpticalSystem>`.
- `--warnaserror+:25` — all new signatures concrete; no generics.
- LF discipline — verify no CRLF churn after edits.
- Baselines: BerremanTests 119, ui-smoke 59, ui-tests 263 unchanged;
  constructor tests 373 → 377 (+4).
