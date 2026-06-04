# Architecture critique -- 004.slice-md cycle 1

## Summary

Clean, well-grounded slice with one or two notes worth the judge's attention.
The materials panel follows the established slice-002/003 view precedent (sub-state
+ dispatchers, never `RootModel`), reuses every named seam, edits no frozen module,
and degrades gracefully headlessly. The single most important item is not a defect
but a documented deviation: the worker correctly discovered the slice spec's central
premise is false (`MaterialPreview.show*` return `unit`, not `GenericChart`) and
routed around it — a defensible §7 call that the judge should bless explicitly.

## Layering

Clean. `MaterialsView.fs` sits in `OpticalConstructor.Ui` and is composed by `Shell`,
which passes `RootMsg.Materials >> dispatch` / `RootMsg.Construction >> dispatch`; the
view takes its sub-state, never `RootModel`, so no module cycle is introduced
(`MaterialsView.fs:305-311`, `Shell.fs:245-251`). It registers AFTER `ChartHosts`/`ChartView`
but BEFORE `Shell.fs` in the fsproj, matching the dependency order. §0.1 is respected:
the only edits to existing files are `Shell.fs` and the two `.fsproj` files, all
new-from-this-arc or explicitly permitted; no frozen model/update/projection module
is touched (confirmed against `git diff` + the untracked-file read).

## Spec fit

R-1/R-3, AC-U3.1, AC-U3.2 are met as specified. The notable item is **R-2**, which
literally requires rendering "the charts returned by `MaterialPreview.show*`." Those
functions delegate to `Analytics.Charting.plotDispersion`, which ends in `|> Chart.show`
(`Charting.fs:92`) and returns `unit` — verified directly. The worker therefore could
not honour the literal wording and instead rebuilt the chart from the engine data
calculator (`Analytics.Variables.calculateN11Re`) via `Chart.Line`, still sourcing the
spectral range through the real `MaterialPreview.spectralRange` seam
(`MaterialsView.fs:241-245`). This is the correct interpretation under the project's
"pick the most defensible reading and record it" rule, and it is recorded in the SoW
Gotchas. I concur with the deviation. One under-delivery to note for the judge's
ledger (not a blocker): the AC-U3.1 intent — Plotly chart → WebView2 host, never an
`AvaPlot` — is fully satisfied, and the bypass loses nothing today because the data
construction is identical to `plotDispersion` minus the side-effect.

## Consistency

The `Filter` / `MaterialsMsg` / pure `update` sub-model mirrors `ChartView`'s chart
sub-state exactly, and the `RootMsg.Materials` plumbing matches the other wired cases
(`Shell.fs:85`, `120`, `197`). Filtering composes only `byCategory` / `byNameContains`
by set intersection — no new index (`MaterialsView.fs:79-88`), matching R-1. Drop
resolution flows through the single `StackEditor.layerMaterialDrop` seam and the view
resolves nothing itself (`MaterialsView.fs:120-130`). The note on the §7 deviation
recurs here: the preview now constructs `Chart.Line` inline rather than behind a
`GenericChart`-returning seam, because none exists. That is consistent with the
codebase as it stands (there is no such seam to follow), so it is not a consistency
defect — but see Evolvability.

## Evolvability

Two forward-looking notes, both low cost. (1) The preview shows only Re[ε₁₁]; the
spec named the whole `show*` family (Xi/N22/N33/Rho…). Because that family is unusable
(all end in `Chart.show`), any future slice that wants the full preview set — or the
live WebView2 bridge — will first need a new `GenericChart`-returning seam in
`MaterialPreview`/`Charting` (the `show*`-returns-`unit` problem will surface again).
Flagging now so the next preview slice budgets that seam rather than discovering it
mid-flight. (2) `referenceWavelength` is hard-pinned to `Templates.defaultLight.waveLength`
(`MaterialsView.fs:100`); the model-driven active-wavelength lift is deferred and
documented. Both previews and drops resolve at that single wavelength, which is fine
for this slice's ACs and cleanly swappable later.

## Risks

**Drag-source vs click-select on overlapping regions.** `entryRow` attaches
`onPointerPressed` → `DragDrop.DoDragDrop` on the `Border`, whose child is the
select `Button` (`MaterialsView.fs:193-202`). Pointer-pressed bubbles from the button
to the border, so a plain click intended to *select* a material for preview will also
enter the modal drag loop on press. On a real (non-headless) surface this can swallow
the button's `Click` (select) or make selection feel like it requires a deliberate
non-drag gesture. No gate catches this because `DoDragDrop`/`onDrop` never fire
headlessly (correctly noted in Gotchas), so the green `ui-tests`/`ui-smoke` do not
exercise it. Smallest fix when this reaches a live surface: start the drag from a
pointer-*moved*-with-button-held threshold, or separate the drag handle from the
select button, rather than initiating on raw press. Worth a hold-for-followup note,
not necessarily a re-spawn.

Minor: `SelectMaterial` can only set `Some id`, never clear selection
(`MaterialsView.fs:68`); harmless today. The two pure tests
(`material drop…`, `materials update…`) are intentionally not wrapped in
`HeadlessSession.run` because they touch only pure functions — correct, and mirrors
`ChartPanelTests` driving `update` directly.

## Bottom line

I would ship this. The implementation is faithful to the achievable spec, respects
all binding constraints (§0.1/§0.2/§0.5), reuses every seam the slice named, and the
one place it diverges from the spec letter is a verified-correct §7 call that is
properly documented. The drag-on-press interaction is a real but headless-invisible
UX risk that I'd record as a followup for the slice that first runs the UI live,
not a reason to block — no AC depends on the live drag path this slice. The judge
should confirm acceptance of the `show*`-returns-`unit` deviation so it is not
re-litigated in a later preview slice.
