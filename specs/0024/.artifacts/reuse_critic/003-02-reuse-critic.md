# Reuse critique -- 003.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on the F# subtree the
  diff touches: `OpticalConstructor.Ui/` views + host, `OpticalConstructor.Ui/`
  frozen logic modules `ConstructionPage.fs` / `StackEditor.fs`, and
  `OpticalConstructor.Ui.Tests/`), plus the `.md` slice / SoW inputs.
- Files inspected: ~10 / 200 (well under the cap; the diff is narrow and
  self-contained, so a full 200-file walk was unnecessary).
- Extensions: task declared `.py,.md,.json`, which do not match this pure-F#
  project. Reuse evidence for an F# diff lives in `.fs` files, so I walked the
  relevant `.fs` helpers directly (the `.md`/`.json` inputs carry no reusable
  code helpers). Noting the mismatch so the judge knows the bound was a
  template default, not a coverage gap.

## Findings

### F1: New `buttonByContent` test helper duplicates the one in `ChartPanelTests.fs`

- **Worker added:** `buttonByContent` (private) in the new
  `ConstructionEditTests.fs:31` — finds the first visual-tree `Button` whose
  `Content` equals a label.
- **Existing helper:** `ChartPanelTests.buttonByContent`, at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ChartPanelTests.fs:23-26`.
  Same `GetVisualDescendants() |> Seq.choose (… :? Button …) |> find-by-Content`
  body, sharing the same `open Avalonia.VisualTree` import.
- **Why it matters:** This is near-miss duplication. The two copies differ only
  in their not-found policy: the existing one returns `Button` via `Seq.find`
  (throws if absent), the new one returns `Button option` via `Seq.tryFind`. The
  new tests genuinely need the `option` form (they assert both presence —
  `(buttonByContent window "Confirm").IsSome` — and absence-tolerant raising via
  `.Value`). So the existing helper could not be reused verbatim, but the
  visual-tree walk itself is identical and now lives in two test modules; a third
  panel test will copy it a third time. Both are `private`, so neither is
  reachable from the other today.
- **Suggested action:** Extract one shared `tryButtonByContent : Window -> string
  -> Button option` (the `option` form is the more general primitive; the `find`
  form is `... |> Option.get`) into the existing shared test-support module
  `TestApp.fs` (which already hosts `HeadlessSession` at `TestApp.fs:42`, the
  module both suites depend on). Then both suites consume one walk. Judge decides
  whether the divergence is worth the extraction this cycle.

### F2: `Shell.parentPath` re-derives the parent-path logic already inlined in `ConstructionPage.removeNodeAt`

- **Worker added:** `parentPath` (private) at `Shell.fs` (new in this diff):
  `match List.rev path with [] -> [] | _ :: revParent -> List.rev revParent`.
- **Existing helper:** the identical `List.rev path` / `last :: revParent` /
  `List.rev revParent` parent-path computation embedded in
  `ConstructionPage.removeNodeAt`, at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionPage.fs:44-48`.
- **Why it matters:** Both compute "the parent path of a `NodePath` by reversing,
  dropping the last branch, and reversing back." The frozen module already owns
  this exact tree-navigation idiom (alongside `tryGetNode`/`updateNodeAt`/
  `removeNodeAt`, the canonical `NodePath` toolbox at `ConstructionPage.fs:26-48`).
  The worker needed it because `ConfirmDeleteNode` must re-solve the *parent* of a
  removed node (the removed path no longer resolves). The risk is low-severity but
  real: if the `NodePath` representation ever changes (e.g. branch ordering), two
  places must move in lock-step, and the new copy sits in a different project
  module from the rest of the path toolbox.
- **Suggested action:** The cleanest reuse — a public `ConstructionPage.parentPath`
  the host calls — would require editing the frozen `ConstructionPage.fs`, which
  §0.1 forbids this slice. So either (a) leave as-is and document in the impl-log
  Gotchas that the duplication is a deliberate consequence of the §0.1 freeze
  (the path toolbox can't be extended this slice), or (b) defer extracting a
  shared `parentPath` to a future slice that is allowed to touch the module.
  This is a constraint-forced duplication, not a careless one; flagging it so the
  judge can confirm the §0.1 rationale is recorded rather than silently accepted.

### F3: `layerRow` re-derives the `"Layer %d — %s"` label format owned by `StackEditor.layerRowLabels`

- **Worker added:** the inline label in `ConstructionView.layerRow`
  (`ConstructionView.fs`): `sprintf "Layer %d — %s" index (StackEditor.displayThickness u layer.thickness)`.
- **Existing helper:** `StackEditor.layerRowLabels`, at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui/StackEditor.fs:217-218`:
  `sys.films |> List.mapi (fun i l -> sprintf "Layer %d — %s" i (displayThickness u l.thickness))`
  — byte-for-byte the same row-label format, and the seam the U1 read-only
  `stackPanel` previously consumed (the old docstring and `PanelViewTests.fs:37`
  both reference it).
- **Why it matters:** Direct duplication of the canonical layer-row-label format.
  The worker correctly reused `StackEditor.displayThickness` (the unit-conversion
  seam) but re-derived the surrounding `"Layer %d — %s"` string that
  `layerRowLabels` already owns, because the edit path needs the live `Layer`
  value (to hang per-layer edit buttons) and `layerRowLabels` returns only
  `string list`, discarding the layers. A side effect: `layerRowLabels` is now
  **unreferenced anywhere in the solution** (grep finds only its definition) — the
  view that used to call it now bypasses it. The orphaning itself is a
  removed-helper concern (out of this critic's scope), but the *label-format
  duplication* is in scope: the format now lives in two modules and the
  `PanelViewTests` "Layer " prefix assertion silently couples to both.
- **Suggested action:** Reusing `layerRowLabels` as-is is impossible without the
  `Layer` (signature mismatch), and extending it to return `(Layer * string)`
  pairs would edit the frozen `StackEditor.fs` (§0.1 — not allowed this slice).
  Defensible options: (a) leave the inline format and note in Gotchas that
  `layerRowLabels` is now superseded by the editable view and a future slice
  should reconcile the two label sites; or (b) have `layerRow` call
  `StackEditor.layerRowLabels u node.system` once and zip the result with `films`
  by index, so the format string stays single-sourced even though the layers are
  threaded separately. Judge decides whether single-sourcing the format is worth
  the zip.

### F4 (minor): `editButton` / CTA buttons echo the `navButton` builder pattern

- **Worker added:** `editButton` (`ConstructionView.fs`, a label + `onClick`
  dispatch button builder) and the inline Confirm/Cancel CTA `Button.create`
  blocks with `Button.background positiveCta`/`negativeCta`.
- **Existing helper:** `Shell.navButton`, at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs:203-209` — a
  label + `Button.background` + `onClick` dispatch builder.
- **Why it matters:** Same "labelled button that dispatches a `Msg`, optionally
  background-tinted" shape, but `navButton` is hard-wired to
  `RootMsg.Shell (Navigate target)` and an active/inactive `SteelBlue`/
  `Transparent` toggle, so it is not reusable for `ConstructionPage.Msg` edits
  without generalising it. This is a weak/low-confidence finding: the two live in
  different modules over different dispatch types, and a shared generic button
  builder would be a *new* abstraction (the rubric forbids proposing helpers that
  don't exist). Recording it only so the judge sees the pattern is recurring; no
  action needed this slice.

## Bottom line

The diff is disciplined about reusing the heavy frozen seams — every stack
mutation routes `ConstructionPage.update`/`StackEditor.applyStackMsg`, and
`displayThickness`, `isNodeBusy`, `confirmationPrompt`, `canUndo`, `solveSubtree`
are all called, not reinvented. The substantive findings (F1 test-helper
duplication, F2 parent-path re-derivation, F3 label-format duplication) are
real but small, and F2/F3 are largely *forced* by the §0.1 freeze that bars
extending the frozen modules this slice — so the right remedy for those is a
recorded Gotcha plus a future reconciliation, not a re-spawn. F1 is the only
finding the worker could fully resolve now (extract one shared
`buttonByContent` into `TestApp.fs`). My read: not re-spawn-worthy on reuse
grounds alone; the judge decides routing.
