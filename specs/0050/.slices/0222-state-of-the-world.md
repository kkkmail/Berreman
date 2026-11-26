# Step 0222 — state of the world (IMPLEMENT: the overlaid item-model store, S72-ITEM-MODEL-OVERLAY)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), lands the shared item contracts before
S7 (001 §H, §S6). Steps 213–221 built:

- the item, its kit file and IO;
- its recording, which snapshots the item model (step 219);
- the app-scope item-model store;
- the copy's item mapping.

This step makes the session's item-model store read a kit's snapshots, as step 142's library, material, sample and
category overlays already do. A kit's items therefore resolve their models on any machine: a model missing locally
is answered from the kit's `.item-model-json` snapshot, read-only (001 §E). The next step (0223) loads a kit's items
into the window. The windows compose the overlaid item-model store beside `KitLibraryStores` at step 243.

## What's working

- Add `ItemModelProxy.overlaidWith`: exact versions resolve to a re-bound held entry, then the kit's snapshot, then the
  store (spec 0050 S7.2).
- List the store's item models first, then the kit's snapshot-only models by name and id. Re-bound and store-held ids
  are left out, and saves pass through to the store.
- Refuse an undecodable snapshot with `ItemModelStoreUnavailableErr`, naming the snapshot and the decoder's reason.
- Add two `KitLibraryTests` facts (empty-library resolution; divergent version and save pass-through). The step-219
  reopen checks now resolve each item's model through the overlay.

## Tests

These are development runs over the final source, recorded in `0222-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0. The final rebuild had 0 warnings and 0 errors. The first full build showed only the two existing
  vendored MathNet `SYSLIB0051` warnings.
- **constructor-unit-tests** (all discovered): 1321 passed, 0 failed, 0 skipped. That is 1319 + 2, the two declared
  identities, each `Passed` in the TRX.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 183 s against its 600 s estimate;
  - suppressed findings stay at 1311.
- **The fourteen `invariant-s22-*` gates:** each holds; their greps find nothing.
- **The facts bite.** Four mutants of the overlay were each caught, then restored byte for byte:
  - store-only resolve;
  - by-id sort;
  - no store-held filter;
  - snapshot models listed first.
- No deadline was exceeded. Not in this step's roster: `unit-tests`, `ui-smoke`, `ui-tests`. No solver or UI code
  changed.

```yaml
gates:
  constructor_unit_tests: 1321
```

## Architecture

- **One overlay rule, reused.** `ItemModelProxy.overlaidWith` sits in `KitLibrary.fs` after step 142's extensions.
  It calls step 142's private `resolveOverlayReference` and `overlayListingSnapshots`, so item models follow exactly
  the precedence and listing rules of materials, samples and categories. Nothing was copied.
- **The pinned reference is the item's own.** Resolution goes through `ItemModelVersionId.entryReference`, the
  exact-version reference `recordItem` snapshots (steps 213, 219). An item reads back the very version it pinned,
  or the kit's divergent copy of it.
- **`toItemModel` is the only decoder** (step 210). There is no second parser for `.item-model-json` content.
- **Composition stays outside `KitLibraryStores`** (unchanged). The overlay adds no cache, no write-back and no other
  member. `saveItemModel` is the store's own field, so the store's `DuplicateItemModelErr` reaches the caller as is.

## Deferred

Nothing in this step's scope is deferred. By the spec's plan:

- loading a kit's items into the windows is step 0223;
- composing the overlaid item-model store in the windows is step 243.

## Gotchas

- **"the store's eight item models" in the slice is stale.** The Built-in catalogue has shipped eleven item models
  since the operator's `0050-210-manual` commit (90ab50b) added the three binding forms: light source, ideal
  polarizer and sample by stack.
  - The fact asserts the store's own listing (`BuiltinCatalogue.itemModels ()` by name and id, eleven), then the
    kit-only models.
  - It keeps the spec's intent: the store first, and no second circular polarizer.
- **A re-bound id answers the held entry's latest envelope**, whatever version was asked for. This is step 142's
  rule, shared by every overlaid store.
- **Built-in name order equals GUID order** for the circular polarizer and the retarder plate. The by-name rule
  therefore bites through the divergent-version fact's two kit-only models, whose names and ids sort in opposite
  orders.
- `KitLibrary.fs` now opens `OpticalConstructor.Domain.ItemModels`. No name was shadowed.

## Changelog

- 2026-11-09 — step 0222: added `ItemModelProxy.overlaidWith` (kit snapshots under the session's item-model store:
  re-binding, then exact snapshot, then store; store-first listing of snapshot-only models by name and id; save
  pass-through; typed snapshot refusal); 2 new `KitLibraryTests` facts and an extended step-219 reopen helper
  (constructor 1319 → 1321).
