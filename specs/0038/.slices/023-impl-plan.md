# Step 023 — impl-plan

## Slice

Spec 0038 Part H, step 023 (IMPLEMENT): **surface entry lifecycle in BOTH catalogue windows**
(Materials + Library), built on the versioned stores landed by steps 021/022 and the lifecycle
foundation of step 020. `touches: [OpticalConstructor.Ui, OpticalConstructor.Ui.Tests]`,
`depends_on: [13, 15, 21, 22]`.

Requirements (from the slice):

1. Verbs **Mark inactive / Mark active / Supersede…** on the selected entry, confirm-gated
   inline, store refusals surfaced as a typed inline message.
2. `ProtectedBuiltIn` **library** entries expose **NO** lifecycle verbs (removed, not greyed).
3. A **show inactive/superseded toggle** (default hidden) adds retired entries to the tree and
   counts, each with a **visible badge**.
4. **Pickers, Select mode, and default facet counts EXCLUDE** inactive/superseded entries;
   **resolution of existing references IGNORES lifecycle** (a bound inactive entry still resolves
   on the table).
5. The view panel **lists an entry's versions**; older versions open **VIEW-ONLY (no Save path)**;
   the library **always edits the latest**.

## Approach (kept entirely inside the two window view files)

The versioned stores (`MaterialProxy` / `SampleProxy`) already expose everything needed:
`listMaterials/listSamples scope` (`ActiveOnly` / `IncludeInactive`), `markMaterialInactive` /
`markMaterialActive` / `supersedeMaterial` (+ sample analogues), and `resolveVersion` (ignores
lifecycle). No Domain change is in scope (`touches` is Ui only), and none is needed — the surface
is complete. So all work lands in `MaterialsWindowView.fs` / `LibraryWindowView.fs` (mirrored) and
their test files.

Design decisions:

- **Show-inactive scope.** New model field `showInactive : InactiveVisibility` (reuse the Domain
  DU — no bool). `projectionInputs` lists the corpus at the *effective* scope: `ActiveOnly` in
  Select mode (Select ALWAYS excludes retired — requirement 4), else `showInactive`. Default
  `ActiveOnly`, so every existing projection (offers, facet counts, Select) is byte-for-byte
  unchanged.
- **Badge.** A retired entry (in the `IncludeInactive` listing but not the `ActiveOnly` set) gets a
  visible ` — inactive` suffix on its tree-leaf label; the toggle carries a count badge
  (`Show inactive (N)` / `Hide inactive (N)`). Supersede and mark-inactive share `InactiveEntry`
  in the store (steps 021/022 gotcha), so the badge reads "inactive" for both.
- **Lifecycle verbs + confirm gate.** A new `lifecycleGate` (parallel to the untouched
  `removeGate`, so the step-013 remove tests stay green) plus a `LifecycleAction` DU. Verbs are
  offered by `offeredLifecycleActions` (a pure, testable helper): an ACTIVE selection offers
  Mark inactive + Supersede…; an INACTIVE selection offers Mark active; NONE otherwise.
  - Materials: `MaterialEntry` carries no protection, so every material is eligible (the material
    store has no built-in guard). Recorded interpretation.
  - Library: only `UserManaged` `SampleItem`s are eligible; `ProtectedBuiltIn` presets (and any
    UserManaged non-sample, which has no version store) offer none — requirement 2.
  - Confirm inline (prompt + Confirm/Cancel); a store refusal (unknown-id race) surfaces as the
    typed inline message (`MaterialError` reused; a new `SampleLifecycleRefused of SampleError`
    for the Library window).
- **Version list + view-only older versions (inline).** The view panel enumerates the selected
  entry's versions by probing `resolveVersion` from v1 upward (the store exposes no list-versions
  field; the probe terminates at the first `None`). Each version is a clickable row; the latest is
  editable through the ordinary Edit verb ("the library always edits the latest"), an older version
  opens **inline, read-only** with a `viewOnlyNote` and no editor/Save path. Inline (not a launched
  editor) because the Sample editor has no view-only mode and the mirror must be identical in both
  windows; recorded interpretation. In the live in-memory store every entry has exactly one version
  (mints need a used version; `VersionsInUse` is empty until step 25), so the multi-version path is
  proven with a stub proxy in tests.

## Files

- `OpticalConstructor.Ui/MaterialsWindowView.fs` — states, model fields, msgs, update arms, pure
  helpers, view rows (toggle, lifecycle verbs, lifecycle confirm, version list + view-only note),
  new UiIds. Composition root `MaterialsWindow.fs` is UNCHANGED (context/init/update/view
  signatures unchanged).
- `OpticalConstructor.Ui/LibraryWindowView.fs` — the mirror, over samples/presets with protection.
- `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` — pure + headless lifecycle tests.
- `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs` — pure + headless lifecycle tests.

## Risks

- `textOf viewPanel` returns the FIRST TextBlock — keep the entry name TextBlock first so the
  step-013/015 "panel contains the name" assertions stay green (version list goes after the
  metadata).
- Model equality: new fields have deterministic defaults, so `Assert.Equal<Model>` comparisons in
  existing tests are unaffected.
- Do not regress the `count_at_least` baselines — every change is additive.
