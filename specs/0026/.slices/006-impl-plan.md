# 006 — Impl plan: ribbon/menu shell, front door, Legacy; catalogue & value-id modal

## Approach

Slice 006 realises **Part D** (ribbon-and-menu shell + single command source) and
**Part F** (standard element catalogue + value-id binding placeholder). Everything
projects from the slice-005 `Commands.registry`; no command is wired twice.

### New modules

- **`Ribbon.fs`** — the office-style ribbon. A pure `tabOf : Command -> RibbonTab`
  classifies every registry command onto one of `Build | ElementTab | TraceView |
  Experiment | Settings`. `tabCommands tab` is the single projection both the
  expanded ribbon and the collapsed drop-down menus render from (constraint 0.4 /
  AC-D2). The contextual `ElementTab` is **appended** to the persistent tabs when an
  element is selected (D.3). The Build tab surfaces the **catalogue** (Optical Table +
  the 8 `CatalogueKind`s — LS/LP/CP/S/Lens/Flat Mirror/Curved Mirror/D, no analyzer,
  F.1) whose element drops dispatch `ConstructorView.RibbonDrop`. The Settings tab
  carries the language/theme selectors and the **Legacy** entry. Ribbon also owns the
  render helpers for the value-id modal overlay and the local-help overlay. The ribbon
  reuses `Controls.fs` button flavours (AC-J1) and takes plain callbacks for
  shell-level actions (Legacy nav / set-language / toggle-theme) so it has **no**
  dependency on `Shell` (acyclic compile order).

- **`LocalHelp.fs`** — context-sensitive help (`HelpContext = ElementHelp of
  CatalogueKind | CommandHelp of Command | TableHelp`); the text resolves through
  `Localization.lookup` (E.2 / J). Pure, no Avalonia type.

### Edits

- **`ConstructorView.fs`** — add `valueIdModalOpen` model state + `OpenValueIdModal`/
  `CloseValueIdModal` messages (the value-id binding action, F.2); fold the modal into
  the `CancelOrDeselect` reset; expose `activeElement : Model -> ElementPlacement
  option` for the contextual Element tab (D.3).

- **`Shell.fs`** — extend `Page` with `Constructor` and `Legacy`; add `constructor :
  ConstructorView.Model`, `ribbon : Ribbon.Model`, `strings : Localization.Resource`
  to `RootModel`; add `RootMsg.Constructor`/`RootMsg.Ribbon` cases and a
  `ShellMsg.SetLanguage`. The front-door nav bar (reusing the `navButton` styling)
  shows **Constructor** (default landing, active-styled) and **Legacy**; the legacy
  pages keep their own sub-nav + lifecycle bar. `initFrom` lands on `Page.Constructor`
  (D.6) and loads the string resource once.

- **`Program.fs`** — document that the constructor-as-default-landing is realised via
  `Shell.initFrom` (which `Program.fs` already mounts); the localization completeness
  surface is unchanged (delivered by slice 003).

- **`strings.json`** — add EN+RU keys for ribbon tabs, commands, catalogue roles, nav,
  the value-id modal, and the help texts (both languages, AC-I3 completeness gate).

### Tests

- **`RibbonTests.fs`** (new): AC-D1 (tab set + registry projection), AC-D2 (ribbon ≡
  menu, every command surfaced once with no second site), AC-D3 (contextual tab
  appended on selection), AC-D4 (bilingual labels), AC-F1 (catalogue roles, no
  analyzer, >2 polarizers), AC-F2 (value-id modal open/close), plus the default-landing
  + Legacy-navigation pure checks and a `ui-smoke` mount of the constructor page.
- **`SmokeTests.fs`**: extend the page list to render the new Constructor + Legacy
  pages headlessly.

## Files to modify

New: `Ribbon.fs`, `LocalHelp.fs`, `RibbonTests.fs`.
Edit: `ConstructorView.fs`, `Shell.fs`, `Program.fs`, `strings.json`,
`OpticalConstructor.Ui.fsproj`, `OpticalConstructor.Ui.Tests.fsproj`, `SmokeTests.fs`.

## Risks

- **Default-landing reroute (high).** Changing `initFrom` to land on `Constructor`
  must not break the existing legacy `Page` paths the `ui-smoke`/`ui-tests` gates
  cover. Mitigation: keep `Construction`/`SynthesisFit` rendering unchanged under the
  Legacy branch; `loadProjectInto` still lands on `Construction` (LifecycleTests:127).
- **AC-D2 single source.** Both ribbon and menu MUST read `tabCommands` (one
  projection of `Commands.registry`); proven by a union-coverage test.
- **Acyclic compile order.** Ribbon must not reference `Shell` — uses callbacks.
- **RU completeness.** Every new key needs a non-empty `ru` (AC-I3 shipped-complete
  test) and no value may equal a neutral scientific symbol (AC-I4).
- **EOL.** The touched UI files are LF-committed (`core.autocrlf=false`, no
  `.gitattributes`; verified `i/lf w/lf` + byte-scan CRLF=0). The Edit tool preserves a
  file's existing EOL, so edits stay LF with no CR churn — do not pre-normalize or force
  CRLF. (Corrects an earlier round's "CRLF-committed" claim, which was stale.)

## Round 3 (retry, cycle 2) — operator issue-hint

Focused single-finding round. The cycle-2 code-judge routed back because the cycle-1
retry applied the "no enabled silent-no-op button" principle to only the nine gesture-only
commands, leaving four element-edit commands `OpenElementDialog` / `ElementContextMenu` /
`ResetRotation` / `DeleteElement` ENABLED on the default landing page with no rendered
front-door surface (Reset/Delete arm an unresolvable `pending`).

Plan: introduce ONE source-of-truth list `ConstructorView.commandsWithoutFrontDoorSurface`
(nine gesture-only + four element-edit) and derive `isParameterlessInvokable` from it;
derive the `RibbonTests` disabled-set from the same list, collapsing the three-way
duplication. The list is broader than `applyCommand`'s inert arms by design (the four
mutate the model), so the seam is a shared list, not model-equality. `Ribbon.commandButton`
(`Ribbon.fs:224`) already keys off the predicate, so only `ConstructorView.fs` +
`RibbonTests.fs` change; `tabOf`/`tabCommands` stay untouched (AC-D1/AC-D2 hold). Add a
rendered-path ui-test asserting the four render disabled on the Element tab; reconcile the
stale CRLF-vs-LF SoW Gotcha to the verified LF ground truth.

Risk: low — narrow, mechanical, reuses the slice's own `disabledButton` pattern; all gates
were already green and stay green.
