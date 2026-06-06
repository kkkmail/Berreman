# 001 — impl-plan: Element geometry, placement & rotation domain model

## Goal

Add the pure, headless-testable element placement / geometry / rotation layer
for Spec 0026 Part A: a new `Placement.fs` module in `OpticalConstructor.Domain`,
the matching schema extension, the per-element placement extension of
`OpticalConstructorProject`, and the A-part geometry tests. No drawing (Part C),
no ray routing (Part B).

## Approach

1. **`OpticalConstructor.Domain/Placement.fs` (new).**
   - `Vector3` — a pure, serializable `{x;y;z}` direction record (NOT the
     Math.NET-backed engine `RealVector3`, which would need a custom JSON
     converter). Keeps geometry provable headless (0.3) and AC-A6 round-trip
     trivial. `dot`, `cross`, `norm`, `normalized`.
   - `centralRayDirection = +X`, `tableNormal = +Z` — the rest-pose reference
     directions (A.3.3).
   - `BoundingBox` — `a1`,`a2`,`b` canonical-meter extents (A×A×B) plus the two
     normals `n1` (primary, ⟂ A×A face), `n2` (secondary). Named role constants
     `opticalFaceExtent/opticalDepth/sourceFaceExtent/sourceDepth` (A.3.4).
   - `CatalogueKind` — 8-case DU of the Part F catalogue roles (A.5.1).
   - `Emission` — three-case DU (`EmitReflectedOnly|EmitTransmittedOnly|EmitBoth`)
     making "both off" unrepresentable; `withReflected`/`withTransmitted` smart
     setters re-enable the other when one is turned off (R-7 / AC-A5).
   - `TablePoint` — `{x;y}` canonical-meter table-plane point (R-2, box centre).
   - `ElementPlacement` — the immutable record (R-1): point, r1/r2/r3 (`Angle`),
     three locks, catalogueKind, `valueId : string option`, box, emission,
     displayUnit. `create` defaults R3 locked, R1/R2 unlocked, valueId None,
     box+emission by role (A.1.2, R-6, R-7.2).
   - Lock-respecting setters `withR1/withR2/withR3` (A.4.5).
   - Geometry: `tableFrameOrientation incidentR2 ownR2 = incidentR2 + ownR2`
     (A.4.3); `orientedBasis`/`orientedNormals`/`r1Axis`/`r2Axis` via a Rodrigues
     `rotateAbout`, composing R3 (about N3, tips out of plane), R2 (about table
     normal), R1 (about face normal). Rest pose → (centralRay, tableNormal)
     (A.3.3); R3≠0 → N1 gains a table-normal component so R1 is no longer ⟂ R2
     (A.4.4).
   - `toConstructorElement : CatalogueKind -> ConstructorElement` (A.5.2): both
     polarizers → `Polarizer`, Sample → `Sample <vacuum placeholder>`, never
     `Analyzer`.

2. **`OpticalConstructor.Domain/Project.fs` (edit).** Add
   `placements : Placement.ElementPlacement list` to `OpticalConstructorProject`
   (per-element placement; the table field stays for slice 004). Nine reserved
   `$defs` anchors untouched.

3. **`schema/optical-constructor-project.schema.json` (edit).** New `$defs`:
   `vector3`, `tablePoint`, `boundingBox`, `catalogueKind`, `emission`,
   `elementPlacement`; new optional root `placements` array. Reuse the existing
   `unitOfMeasure` `$def` for displayUnit. The permissive `constructorElement`
   anchor and the nine reserved names are left intact (A.8.1).

4. **`OpticalConstructor.Tests/PlacementTests.fs` (new).** AC-A1..A5, A7 headless
   geometry/mapping tests + AC-A6 placement round-trip & schema validation.

5. **fsproj wiring.** Register `Placement.fs` (after `BeamTree.fs`, before
   `Project.fs`) and `PlacementTests.fs`.

6. **Update the 8 existing `OpticalConstructorProject` literal constructions** to
   add `placements = []` (Templates.fs, SystemView3DTests, ProjectJsonRoundtrip,
   RoundTrip, History, ExportImport, StackEdit).

## Risks

- Adding a mandatory record field breaks every literal construction site — must
  find/patch all (8 found via `beamTree =` grep; `{ p with ... }` copies are fine).
- Schema must match FSharp.SystemTextJson output exactly: `Angle` (single-case
  union) → bare number; fieldless DUs → bare strings; `Vector3`/`BoundingBox`
  records → objects; `option` → value-or-null. Verified against the existing
  serializer options (`ProjectJson.fs`).
- R3 non-orthogonality (A.4.4) is the numerical care-point; Rodrigues about the
  N3 axis tips N1 out of the table plane so AC-A4 holds.
