# Step 014 — impl-plan

## Goal

Give `SmpMakeMultilayer` a **distinct launcher path** so it opens a NEW sample
**pre-seeded with a foldable 2-layer period** (distinct from the blank `SmpAdd`
and from the `Edit`/update-in-place path). Save still mints a fresh `SampleId`
through `SampleProxy.addSample`. Verified by headless tests in
`OpticalConstructor.Ui.Tests`.

## Approach

The Sample editor's opening mode was encoded as a bare `Sample option` at three
seams (`SampleEditorView.init`, the `SampleEditorWindow` ctor, and the
`EditorLaunchers.openSampleEditor` field): `None` = blank Add, `Some s` = Edit.
A third mode is needed. Per the repo "no naked bool / elevate every primitive"
discipline, elevate that `Sample option` to a 3-case DU:

```fsharp
type SampleEditorIntent =
    | NewBlankSample
    | NewSeededMultilayer
    | EditSample of Sample
```

Both NEW intents map to the existing `EditorTarget.NewSample` (Save mints a
fresh id); they differ only in the seeded structure. `Edit` keeps
`ExistingSample s.id`.

## Files to modify

1. **`OpticalConstructor.Domain/ElementId.fs`** (`Library` module) — add a pure
   `starterMultilayerStructure : SampleStructure`: ONE `Repeated` period of a
   2-layer glass(1.52)/vacuum cell at the existing private λ/4-at-600 nm
   thicknesses (`multilayerQw`'s unit cell), `count = 1`. Bare structure DATA,
   not a seeded `Sample`.
2. **`OpticalConstructor.TestWindows/SampleEditorView.fs`** — add
   `SampleEditorIntent`; change `init` to branch on it (blank → empty; seeded →
   `starterMultilayerStructure`; edit → existing).
3. **`OpticalConstructor.TestWindows/SampleEditorWindow.fs`** — ctor 3rd arg is
   the intent; Title per intent; `init` threaded the intent.
4. **`OpticalConstructor.TestWindows/TableAndElementRotationView.fs`** —
   `openSampleEditor` field/`defaults` take the intent; split
   `SmpAdd | SmpMakeMultilayer` into `SmpAdd` → `NewBlankSample` and
   `SmpMakeMultilayer` → `NewSeededMultilayer`; `SmpEdit` → `EditSample sample`.
5. **`OpticalConstructor.Ui.Tests`** — migrate existing `None`/`Some` call sites
   in `SampleEditorWindowTests.fs` and `MainWorkbenchTests.fs` to the intent
   cases; the `recordingLaunchers` helper distinguishes `sample-multilayer`;
   add a pure test (seeded init) + headless tests (SmpMakeMultilayer opens the
   editor showing the seeded 2-layer period; Save persists via `addSample`).

## Risks

- **Blast radius:** the `Sample option` seam is referenced by ~15 test call
  sites. All migrations are mechanical (`None`→`NewBlankSample`,
  `Some x`→`EditSample x`). MainWorkbenchTests qualifies the cases
  (`SampleEditorView.X`) to avoid shadowing its `open TableAndElementRotationView`.
- **Save on a blank name:** the seeded structure is non-empty, so
  `validateSample` only rejects a blank name — the headless Save test sets a
  name first.
- Gates are run by the arc-runner after exit (Invariant 6); this session runs
  no gate commands.
