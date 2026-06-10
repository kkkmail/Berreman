# 004 — Impl plan: Optical table, drawer & top-down rendering geometry; standardized controls

## Approach

Slice 004 realises **Part C** (table + drawer + top-down rendering geometry) and
**Part J** (standardized controls) of Spec 0026, as PURE, headless-testable
projections on top of slice 001 (`Placement.fs`) and slice 002 (`RayModel.fs`).
No FuncUI `Canvas` page, no interaction gestures (slice 005), no ribbon controls
(slice 006). The existing `Schematic.fs` (a *vertical cross-section* view) is left
untouched; the top-down surface is net-new.

Four new modules + two edits + two test files:

1. **`OpticalConstructor.Domain/Table.fs`** (new) — the `OpticalTable` plate record
   (width/length/thickness in canonical SI meters + display unit; default
   `1.2 × 2.0 × 0.10 m`, editable `withSize`) PLUS the pure `TableViewState`
   (table R1/R2/R3 relative to the screen, pan, zoom; default top-down `(0,0,0)` +
   `defaultZoom`, `reset` target). Compiles before `Project.fs`.

2. **`OpticalConstructor.Ui/Controls.fs`** (new) — the single standardized
   control-flavours module: pure `ButtonFlavor` style records (default / primary /
   destructive) built by overriding only the distinguishing properties (J.1.2), the
   FuncUI flavour functions (`defaultButton` / `primaryButton` / `destructiveButton`
   / `toggle` / `numericWithUnits`), and the **destructive-gate** flavour carrying
   the positive/negative CTA colours. `ConstructionView.fs:40` `positiveCta` /
   `negativeCta` are rewired to draw from here (one definition — J.2 / R-9).

3. **`OpticalConstructor.Ui/ConstructorTable.fs`** (new) — the named drawing-weight
   constants (C.5.1, single-edit restyle), the top-down layout geometry (plate to
   scale + view transform driven by `TableViewState`), the ray-group stroke map
   (CR 2px/full, side 1px/~60%, reflected ~35%), the active-element indicator
   (≥2px, ≥3:1 WCAG contrast vs the plate — pure `contrastRatio`), and the CR-only
   default (`showCentralRayOnlyDefault = true`). Reuses `SchematicColor`.

4. **`OpticalConstructor.Ui/Drawer.fs`** (new) — the ONE standard cylinder drawer:
   `draw point r1 r2 r3 kind valueId showBox → DrawerGeometry`. Reuses
   `Placement.orientedBasis` for the rotations (no duplicated math); cylinder
   silhouette + cap axis in table-plane meters; light-grey transparent interior,
   darker source shade, near-black detector with a still-visible frame; 12 projected
   bounding-box edges when `showBox` is on (off by default). Consumes the frame /
   interior constants from `ConstructorTable.fs`.

5. **`OpticalConstructor.Domain/Project.fs`** (edit) — add `table : Table.OpticalTable`
   to `OpticalConstructorProject` (sibling of slice 001's `placements`).

6. **`schema/optical-constructor-project.schema.json`** (edit) — add the optional
   `table` root property + the `opticalTable` `$def` (width/length/thickness numbers
   + `unitOfMeasure`). Reserved anchors untouched.

7. **`OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs`** (edit) — AC-C1: the
   edited table size round-trips through `serializeProject`/`deserializeProject`
   (schema-validated on load).

8. **`OpticalConstructor.Ui.Tests/ConstructorViewTests.fs`** (new, `Category=ui-tests`)
   — drawer geometry + shadings + show-box (AC-C4), active-element indicator
   threshold (AC-C5), CR-only default (AC-C6), control-flavour usage (AC-J1), plus
   the table default size + view-state defaults. All pure logic (no headless render).

Every full `OpticalConstructorProject` literal (9 sites) gains
`table = OpticalConstructor.Domain.Table.defaultTable` — the same mandatory-field
sweep slice 001 did for `placements`.

## Risks

- Adding a mandatory `table` field breaks every project literal — must sweep all 9
  construction sites (1 production: `Templates.fs:78`; 8 in tests).
- The AC-C5 ≥3:1 contrast must be asserted from pure colour values — a dark, strongly
  saturated indicator on the light-grey plate clears 3:1 (computed in-code so the test
  uses the same `contrastRatio`).
- Keep `Schematic.fs` untouched; reuse `SchematicColor` only.
- EOL: the Edit/Write tooling emits CRLF on this host (repo is LF); normalize every
  touched file back to LF before exit.

## Gates

`build`, `unit-tests` (BerremanTests untouched → baseline 84 holds),
`constructor-unit-tests` (+1 AC-C1), `ui-tests` (+ConstructorViewTests), `ui-smoke`
(new modules linked, existing views unchanged), `impl-log-structure`,
`state-of-world-structure`.
