# 002 — impl-plan: Ray model, snapping & schematic tracing (Part B)

## Approach

Add one new pure-geometry domain module
`OpticalConstructor.Domain/RayModel.fs` and its headless test file
`OpticalConstructor.Tests/RayModelTests.fs`. The module carries **no
Avalonia type** (constraint 0.3); every rule is provable headless from the
test project. It sits ABOVE `Placement.fs` (slice 001) and `BeamTree.fs`,
reusing both rather than re-typing geometry:

- `Placement.Vector3` is reused for ray directions and (documented as
  canonical meters) ray-bundle positions; `Placement.r1Axis` /
  `orientedBasis` supply the element face normal so the R2/R3 rotation law
  is not duplicated; `BeamTree.BeamBranch` (`Reflected`/`Transmitted`) is
  the engine branch the drawn RRG/TRG correspond to (B.7.1, AC-B2).

Public surface (the hand-off contract for slices 004/005):

- **Named constants** — `bundleRadiusFraction = 0.4` (B.1.2),
  `sideRayCount = 8`, `refractiveIndexOutside = 1.0` /
  `refractiveIndexInside = 1.5` (B.6.1), `defaultSourceDetectorDistance =
  2.0<meter>` (B.4.1), `resnapDistance = 0.02<meter>` (B.5.3, configurable
  via `restoreRayWithin`).
- **Ray bundle (B.1 / AC-B1)** — `Ray`, `RayBundle`, `bundleRadius`,
  `bundleAt`: one central ray + eight side rays as the 45°-spaced
  "rifle-target" circle of radius `0.4 × face_extent/2` in the cross-section
  perpendicular to the ray.
- **Ray groups (B.2 / AC-B2)** — `RayGroupKind` (`Incident`/`Reflected`/
  `Transmitted`), `emittedGroups`, `outgoingGroups` (detector terminates,
  B.2.2), `toBeamBranch` (engine correspondence).
- **Snell tracing (B.6 / AC-B6)** — `faceNormal`, `reflect`, `refract`,
  `outgoingDirection`: vector reflection and Snell refraction with
  n=1.0/1.5; bends the CR and side rays through a tilted element.
- **Snapping + downstream travel (B.3 / AC-B3)** — `RaySegmentSpec`,
  `SnappedElement`, `RayChain`, `snapChain`/`snap`: walks the ray polyline
  by fixed along-ray gaps so tilting an upstream element re-aims everything
  downstream while preserving every along-ray distance.
- **Default placement + reset (B.4 / AC-B4)** — `defaultSourcePoint`,
  `defaultDetectorPoint`, `moveSource`, `resetSource`.
- **Orphaning (B.5 / AC-B5)** — `RayId`, `RayAttachment`, `removeRay`,
  `restoreRay`/`restoreRayWithin`, `withPlacement`: only ray-removal /
  branch-switch orphans; restore re-snaps iff within `resnapDistance`.

## Files to modify

- New: `OpticalConstructor.Domain/RayModel.fs` (insert after `Placement.fs`
  in `OpticalConstructor.Domain.fsproj`).
- New: `OpticalConstructor.Tests/RayModelTests.fs` (insert after
  `PlacementTests.fs` in `OpticalConstructor.Tests.fsproj`).
- No edits to existing source (slice files-in-scope list "Edited: none").

## Risks

- The distance-preserving downstream transform (B.3.2/AC-B3) and the Snell
  bend (B.6/AC-B6) are exact headless assertions — float tolerance follows
  the project precedent (local `abs (a-b) <= eps`, as `PlacementTests.fs`).
- Orphaning edge cases (restore-within-distance) pinned precisely by tests.
- `--warnaserror+:25` (incomplete matches) — all DU matches kept total.
