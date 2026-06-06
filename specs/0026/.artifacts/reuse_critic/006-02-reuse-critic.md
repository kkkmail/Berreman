# Reuse critique -- 006.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (the `Berreman/OpticalConstructor` subtree — `…Ui`, `…Ui.Tests`, `…Domain`).
- Files inspected: ~16/200 in depth — the new/edited `.fs` (`Ribbon.fs`, `LocalHelp.fs`, `Shell.fs`, `ConstructorView.fs`, `Controls.fs`, `RibbonTests.fs`, `SmokeTests.fs`) + the existing helpers they touch (`Commands.fs`, `Localization.fs`, `Help.fs`, `TestApp.fs`, the sibling panel tests, `strings.json`).
- Extensions: the pinned `.py,.md,.json` set does not fit this pure‑F# repo; I walked the relevant `.fs` reuse surface plus `strings.json`. No `max_files` cap hit.

(Cycle‑2 context — this supersedes the cycle‑2 critique at this path. Cycle‑2's headline F1 is **fixed**: the inert / no‑front‑door‑surface command set is now ONE list, `ConstructorView.commandsWithoutFrontDoorSurface` (`ConstructorView.fs:438-453`), from which `isParameterlessInvokable` and the `RibbonTests` disabled‑set both derive — the prior three‑way duplication is collapsed. The findings below are the remaining / new ones.)

## Findings

### F1 (new this cycle): Headless mount-and-query test scaffolding re-rolled again in `RibbonTests`

- **Worker added:** `RibbonTests.buttonLabels` (`OpticalConstructor.Ui.Tests/RibbonTests.fs:67-80`) plus the inline "create `Window`, `Content <- Component …`, `Show()`, `RunJobs()`, walk `GetVisualDescendants()` and project `Button`s" blocks in the two enabled‑state tests (`RibbonTests.fs:281-301` and `:313-333`).
- **Existing helper:** the same scaffolding already exists, privately, in nearly every sibling panel test — `MaterialsPanelTests.buttonContents` / `mount` / `textBlocks` (`MaterialsPanelTests.fs:28`, `:42`, `:34`), `FitPanelTests.mount` / `buttonByContent` / `hasButton` (`FitPanelTests.fs:67`, `:74`, `:79`), and the identical `mount`+`GetVisualDescendants` pattern in `ConstructionEditTests.fs:32/45`, `ChartPanelTests.fs:24/35`, `PanelViewTests.fs:21/29`. `buttonLabels` is functionally identical to `MaterialsPanelTests.buttonContents` (collect every `Button.Content` as a `string list`).
- **Why it matters:** the "mount a FuncUI view in a headless `Window` and read the visual tree" idiom is now copy‑pasted across ~6 test modules; RibbonTests is the latest copy, and it duplicates the idiom *internally* too — `buttonLabels` collects labels, but the two enabled‑state tests can't use it (they need `(label, IsEnabled)` pairs) so they re‑inline the whole mount + descendant walk twice more. Any change to the idiom (a missing `window.Close()`, a second `RunJobs()`, a threading tweak) has to be repeated in every copy.
- **Suggested action:** extract one shared headless render‑and‑query helper next to `HeadlessSession` in `TestApp.fs` (e.g. a `mount` plus a `buttonsByContent : Window -> (string * bool) list`) for all panel tests to consume — *or* accept that this duplication predates the slice and is pervasive, leaving RibbonTests consistent with the established house style and consolidating separately. Consistency with the existing (already‑duplicated) convention is a defensible reason to leave it; the judge decides.

### F2 (carry-forward, unaddressed): Resource load + empty-fallback re-implemented in Shell and in the test

- **Worker added:** `Shell.emptyStrings = { entries = Map.empty }` and `Shell.loadStrings ()` (`Shell.fs:187-201`), wrapping `Localization.loadFromFile (Localization.resourcePath ())` with an empty‑resource fallback; the *same* recipe appears verbatim in `RibbonTests.resource` (`RibbonTests.fs:50-53`).
- **Existing helper / owner:** the `Localization` module owns `Resource`, `loadFromFile` (`Localization.fs:127`), `resourcePath` (`Localization.fs:94`), and already pairs them inside `startupCheck` (`Localization.fs:197-202`), which `Program.fs` runs at launch.
- **Why it matters:** "load the shipped resource from the canonical path, fall back to `{ entries = Map.empty }`" is now stated in three places across Ui and Ui.Tests, and the canonical‑path load already happens (then is discarded for its error) inside `startupCheck` — i.e. a redundant second disk read of `strings.json` on every launch. The empty‑resource constant and the load‑or‑empty fallback belong in the module that owns `Resource`. (This was cycle‑2 F2 / cycle‑1 F3 and remains unaddressed.)
- **Suggested action:** add a `loadOrEmpty ()` (and the empty‑resource value) to `Localization.fs`; call it from `Shell.initFrom` and the test, and thread the one parsed `Resource` from the composition root into both the completeness surface and the shell so the file is read once. Lower priority than F1; a test fixture loading its own resource is a defensible norm.

### F3 (carry-forward, low confidence): A second exhaustive command→string table parallel to the registry `id`

- **Worker added:** `LocalHelp.commandLabelKey : Command -> string` (`LocalHelp.fs:43-68`), an exhaustive, wildcard‑free match from every `Command` to a `strings.json` label key.
- **Existing helper:** the registry already declares one stable per‑command string — `CommandDef.id` (`Commands.fs:151-158`) — exposed by `Commands.idOf` (`Commands.fs:296`).
- **Why it matters:** to its credit the label keys reuse the pre‑existing generic `command.*` entries (`command.save/undo/redo/copy/paste/delete`, `strings.json`), so this is *not* key duplication. But the *function* is a second exhaustive command‑keyed table: unlike `Ribbon.tabOf` (which ends in a load‑bearing wildcard, `Ribbon.fs:97-99`), `commandLabelKey` compile‑forces a new arm for every future command, so a slice‑007 command is a registry entry **and** a `commandLabelKey` arm, not the "one entry, no second wiring site" the SoW frames. (The exhaustiveness is at least compiler‑enforced, so the coupling can't silently drift.)
- **Suggested action:** consider keying labels off the existing `id` (derive the `strings.json` key from `Commands.idOf`) so the per‑command handle lives once in the registry. Low confidence — the id (`"delete-element"`) and label‑key (`"command.delete"`) spaces legitimately differ and the worker's shared‑key reuse is genuinely good, so this is a consolidation opportunity, not a defect.

## Bottom line

The diff stays a strong reuse citizen and the cycle‑2 headline (the three‑way inert‑command list) is genuinely fixed down to one source — ribbon + menus still project from the one `Commands.registry`, the catalogue maps through the existing `Placement.toConstructorElement`, buttons reuse `Controls` flavours (I also re‑examined `Controls.disabledButton` bypassing the `button` constructor and `LocalHelp.fs` coexisting with `Help.fs`, and judged both below the bar), and nav extends the existing `Page`/`RootModel`/`RootMsg`. The one finding I'd weight is the new F1 — the headless mount/collect test scaffolding is re‑rolled a sixth time with citable existing twins, though the duplication is pre‑existing and pervasive so matching the house style is defensible; F2 is a clean unaddressed carry‑over near‑miss and F3 is advisory. None of this is a correctness or contract problem, so my read is cleanup‑suggestion rather than re‑spawn material — but the routing call is the code judge's.
