# Step 011 — state of the world

## Where we are

Spec 0040 Part D, step 011 (IMPLEMENT) — the D.4 rule "let a film-less Plate set its
substrate through the Materials window". Before this slice a sample's substrate plate
(`SampleStructure.substrate`) could only be set "from chosen" — the toolbar's chosen
material — and the ONLY way to choose a material is a film row's *Choose material…*
verb. A film-less Plate has no film rows, so it could never pick a substrate material
at all. This step gives the substrate slot its own *Set…* verb that opens the Materials
window in Select state targeted at the substrate, mirroring the per-layer material pick.
Scope: `OpticalConstructor.Domain` (the target DU) + `OpticalConstructor.Ui` (the two
window banners + the sample editor) + `OpticalConstructor.Ui.Tests`; `depends_on: [10]`.

## What's working

- Add a third `SelectionTarget` case `SampleSubstrateTarget` (no payload — one un-positioned substrate slot per sample) beside `SampleLayerTarget`.
- Give the substrate slot its own `setSubstrateVerb` (Set…) that opens the Materials window in Select state targeted at `SampleSubstrateTarget`, modelled on `chooseMaterialForLayer` / `chooseMaterialVerb`; it needs no pre-chosen material, so a film-less Plate can now pick a substrate.
- Route the targeted Select return through a new `BindMaterialToSubstrate` message onto `SampleStackMsg.SetSubstrate (Some layer)`; the picked id also becomes the toolbar's chosen material (the `BindMaterialToLayer` precedent).
- Resolve the new target's banner prose in both the Materials and Library window `targetText`s ("the requesting sample substrate plate").
- Replace the `hasChosenMaterial`-gated substrate 'Set from chosen' button (and remove its now-dead `SetSubstrateClicked` message + arm).
- Add a headless `ui-smoke` proof (a film-less Plate picks its substrate through the real Materials Select window) and a pure `ui-tests` fact for `BindMaterialToSubstrate`.

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker runs no gates. Local
verification (to avoid a non-building/hollow result) and how the change lands per gate:

- `build` — VERIFIED green (`dotnet build Berreman.slnx -c Release` → `Build succeeded.
  0 Error(s)`; the only warnings are pre-existing and not from our F# code: the exempt
  `NU1701` Wolfram advisory and `SYSLIB0051` in the vendored MathNet C# source). The
  new `SelectionTarget` case's two exhaustive-match breaks (`targetText` in the
  Materials + Library window views) are both resolved.
- `unit-tests` (BerremanTests) — solver core; does not reference OpticalConstructor;
  count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — VERIFIED 686 passed / 0 failed;
  the Domain change is a pure additive DU case, behaviour unchanged.
- `ui-smoke` — 1 net-additive headless fact: a film-less Plate opens the Materials
  window in Select state from the substrate Set… verb (banner names the substrate),
  picks 1.75 glass, and the substrate summary shows it. Count rises by 1.
- `ui-tests` — 1 net-additive pure `BindMaterialToSubstrate` fact + 1 id-contract assert
  on the existing UiId fact. Count rises by 1.

Acceptance mapping (D.4): "a film-less Plate MUST be able to set its substrate by picking
a material through the Materials window in Select state targeted at the substrate slot —
headless-verified" → the headless `acceptance (011): a film-less Plate sets its substrate
through the Materials Select window`, backed by the pure `BindMaterialToSubstrate sets the
substrate plate to the picked material and records it as chosen`.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- **Reuse the step-019 layer-pick shape, not a new mechanism.** The substrate Set…
  verb is the substrate analogue of the per-layer *Choose material…* verb: a
  `SelectionContext<MaterialEntry>` over the same `openMaterialsSelect` context seam,
  `onSelected` baking a targeted return, both outcomes re-querying via `RefreshMaterials`.
  No new IO, no new window, no new launcher — the composition root's generic
  `openMaterialsSelect` handles any target.
- **The substrate target is payload-free by design.** `TableElementTarget` /
  `SampleLayerTarget` carry a typed identity because their targets are positional; a
  sample holds exactly ONE substrate plate, so `SampleSubstrateTarget` carries nothing —
  the return routes through the captured dispatch. Honest to the domain (a nullary case,
  like `Browse`) and no invented primitive.
- **Set always succeeds; robustness is inherent.** `SampleStackMsg.SetSubstrate` never
  rejects, so `BindMaterialToSubstrate` always lands for a live editor; the only no-op is
  a closed editor's dead dispatch. The `applySampleStackMsg` totality's `Error` arm is
  kept as the defensive status-line path rather than assumed away.
- **No serialized-contract change.** `SelectionContext` is `[<ReferenceEquality>]` and
  the added target case is in-memory only; `Sample` / `SampleStructure` are unchanged, so
  no store or schema touch.

## Deferred

- A substrate Set… affordance for the lower half-space (still `hasChosenMaterial`-gated
  'Set from chosen') — out of this slice's scope, which is the substrate slot only.
- Downstream consumption of the substrate plate in the beam-tree / engine mapping — this
  slice only wires the editor's pick path; the plate is a Domain field as before.

## Gotchas

- **Adding a `SelectionTarget` case breaks BOTH `targetText`s** (Materials + Library
  window views) — each matches the DU exhaustively, so both got the new arm or FS0025
  (the warning-as-error class) fails the build. No other exhaustive match over
  `SelectionTarget` exists (grep-verified).
- **The substrate target carries no payload** (one un-positioned substrate per sample):
  "a vanished editor is a no-op plus status line" reduces here to a closed-window dead
  dispatch plus the totality's defensive `Error` arm — there is no film-row-style
  vanished slot to validate for a single substrate.
- **`SetSubstrateClicked` was removed, not left dead** — its only trigger (the replaced
  substrate 'Set from chosen' button) is gone; the message + arm were removed together to
  keep `update` exhaustive. Grep confirmed nothing else referenced it.
- **The Set… verb resolves its owner `Window` from the click's visual tree**
  (`TopLevel.GetTopLevel e.Source`) — a modal Select open needs the owner, so it cannot
  use the plain `verbButton` (unit-onClick); it follows the proven `chooseMaterialVerb`
  pattern, reuses the `setSubstrateButton` id, and is always enabled (no chosen-material
  gate — that gate was the film-less-Plate gap).

## Changelog

- 2026-07-13 — Step 011: let a film-less Plate set its substrate through the Materials
  window in Select state — add `SelectionTarget.SampleSubstrateTarget`; a substrate
  `setSubstrateVerb` (Set…) opening the Materials Select window; a `BindMaterialToSubstrate`
  return onto `SetSubstrate (Some layer)`; the new-target banner arm in both window
  `targetText`s; replace/remove the chosen-gated substrate button; +1 headless (`ui-smoke`)
  and +1 pure (`ui-tests`) proof. Build + Ui.Tests (672) + constructor-unit-tests (686)
  verified green locally.
