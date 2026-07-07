# Impl log — spec 0033, slice 015 (ADD_COMPONENT UICOMP_XDUO_0001 MaterialsControls)

## Progress

- [x] Read task file, worker system prompt (add_component_worker + the
      arc-runner base), project prompt, slice spec, slice-014 SoW/impl-log,
      LibraryControls.fs, ElementPaletteControls.fs, RayPositionControls.fs,
      ExperimentControls.fs, the two fsprojs, LibraryControlsTests.fs,
      RayPositionControlsTests.fs, ExperimentControlsTests.fs, TestApp.fs,
      and the domain MaterialQuery / MaterialCategory / DispersionFilter seam.
- [x] Red: MaterialsControlsTests.fs + its fsproj entry added first; the
      build fails with 130 × FS0039 naming the missing `MaterialsControls`
      (`015-red-tdd.log`).
- [x] Implementation: MaterialsControls.fs (new, ~280 lines) + the Controls
      fsproj entry.
- [x] Green: all five gates pass in the advisory local runs on the FIRST
      attempt after red — no test edits after red.
- [x] Contract registry: `specs/0033/.contracts-json` already records
      `UICOMP_XDUO_0001` as `declared` / `declaringStep: 15`
      (supervisor-maintained) — nothing for the worker to write.
- [x] LF policy verified (see Gotchas).
- [x] State-of-the-world written.

## Decisions

1. **Shape copied from `LibraryControls`, ids as `[<Literal>]`s.** Pure
   `Row` / `State` records, a `Handlers` record of unit-returning
   functions, one `[<RequireQualifiedAccess>] module UiIds`. The eight
   slice-mandated ids are `[<Literal>]`s (per the slice letter); the
   derived per-row / per-facet-option ids are prefix functions
   (`MaterialRow_` / `MaterialCategoryOption_` / `MaterialDispersionOption_`),
   the `LibraryControls.UiIds.entry` precedent.
2. **Editability is a two-case DU, not a naked bool.** CLAUDE.md's
   elevate-every-primitive rule ("no naked bool in a domain record")
   outranks the older siblings' `isBound : bool` habit for NEW code:
   `MaterialEditability = Editable | ViewOnly`, so the verb-row match
   reads as prose. "View-only" = a material with no edit model (spec 0033
   step 013: the `complexity = None` built-ins).
3. **Edit is REMOVED — not greyed, not hidden — for a view-only
   selection.** The verbs are one toolbar acting on the selected row (the
   slice names ONE `EditMaterialButton`, so the affordance cannot be
   per-row): `Some { editability = ViewOnly }` produces an empty Edit
   segment in the verb-row child list. With no selection Edit is present
   but disabled (like Remove / View); only a view-only SELECTION removes it.
4. **AutomationId (not `Control.Name`) on every control in a
   variable-membership child list** — the rows (the host's filter rewrites
   them), the facet options (host-supplied lists), and the four verb
   buttons (Edit leaves/returns, shifting Remove/View across slots).
   Avalonia forbids renaming a styled control, so FuncUI recycling across
   shifted slots throws on `Name` but not on the freely-mutable
   `AutomationProperties.AutomationId` (the documented
   `ExperimentControls.idOptionBox` precedent). Fixed structural controls
   (the search TextBox, the two facet WrapPanels, the rows StackPanel)
   keep `Name`. Tests match by Name OR AutomationId (the `matchesId`
   idiom from ExperimentControlsTests).
5. **Domain-free state mirrors the `MaterialQuery` seam.** `searchText` +
   `selectedCategory` + `selectedDispersion` echo the domain query's
   text/category/dispersion facets as host-supplied `code` strings
   (`FacetOption = { code; label }` — the `AddItem` id/label precedent),
   so the later wiring step maps codes back to `MaterialCategory` /
   `DispersionFilter` without this project referencing Domain. The host
   includes its own "all" option — the control hard-codes no facet.
6. **The control renders the rows it is given** — no filtering here; the
   search box uses the live `TextBox.onTextChanged` idiom the Ui materials
   panel already uses for search (MaterialsView.fs:169), dispatching
   `setSearchText` so the HOST re-runs its search seam.
7. **Component declared, not wired** (ADD_COMPONENT obligation): no parent
   view, TestWindows host, or Ribbon bay references MaterialsControls this
   round; the headless tests mount the component alone in a plain Window.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/MaterialsControls.fs`
  (new) — the component: `MaterialEditability`, `Row`, `FacetOption`,
  `State` + `empty`, `Handlers`, `UiIds`, the pure `selectedRow`, and the
  `view` projection (search row, two facet selectors, the rows list, the
  verb row), styled to MATCH the sibling bars' idle/chosen palette.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
  — `<Compile Include="MaterialsControls.fs" />` after ExperimentControls.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsControlsTests.fs`
  (new) — 3 pure contract tests (`ui-tests` gate): empty state, the stable
  intent-named ids + derived prefixes, `selectedRow` resolution; 2 headless
  structure tests (`ui-smoke` trait): (a) mount over the known State, every
  UiIds control present, Add click dispatches ONLY the add handler, then
  Edit / Remove / View / a row / both facet options each dispatch their
  matching stub handler; (b) a view-only selection removes
  `EditMaterialButton` from the tree entirely while Add / Remove / View
  remain.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — `<Compile Include="MaterialsControlsTests.fs" />` after
  LayerBandsControlsTests.fs.

## Testing state

TDD: red first (130 × FS0039 naming the missing `MaterialsControls`,
`015-red-tdd.log`), then the production module, then green on the first
run with no test edits after red. All five gates in the slice roster pass
in the worker's local ADVISORY runs (per Invariant 6 the arc-runner gate
engine is the sole gate authority and re-runs them after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors, no
  warnings from the new files.
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; this slice
  adds no Domain code).
- `ui-smoke` — 56 passed, 0 failed (54 baseline + the 2 new headless
  structure tests).
- `ui-tests` — 252 passed, 0 failed (249 baseline + the 3 new pure
  contract tests).

Acceptance check (inside the ui-smoke run): the headless test mounts
MaterialsControls over a known State, finds every UiIds control, and a
simulated Add click invokes the add handler (and no other verb handler).

## Artifacts

- `specs/0033/.artifacts/015-red-tdd.log` — red build (FS0039 × 130).
- `specs/0033/.artifacts/015-build.log` — solution build.
- `specs/0033/.artifacts/015-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/015-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/015-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/015-ui-tests.log` — ui view tests.

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does not
  exist; the file lives at
  `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\multistep\add_component_worker.system-md`
  (found by glob; matches the layout the base protocol describes). Read it
  from there.
- **Invariant 6 vs the base protocol's "run gates locally":** the
  add_component_worker delta says the worker runs no gates; the arc's
  established contract-family precedent (slice 003) still runs them as
  ADVISORY local verification because the SoW's structural gate requires
  the `gates:` baseline YAML counts. Followed the precedent; the counts
  reported here are advisory only.
- **Line endings:** the two edited fsprojs show ZERO CRLF churn
  (`git diff --numstat` identical with and without `--ignore-cr-at-eol`).
  The two NEW files are CRLF on disk — exactly like every committed
  sibling in this working tree (e.g. LibraryControls.fs is CRLF on disk) —
  and `.gitattributes` (`*.fs text eol=lf`) normalizes them to LF blobs at
  `git add`, so the repository stays LF.
- **Do not "grey" the Edit verb later:** removal-not-greying for view-only
  selections is a slice-letter requirement and a headless test pins it
  (the button must be absent from the visual tree, Name AND AutomationId).
- The Edit verb's leave/return is safe under FuncUI diffing ONLY because
  the verb buttons carry AutomationId, not Name (Decision 4) — keep that
  if the verb row is ever reordered or extended.
- `.manifest.state.json` and the untracked `.claude/` folder show in
  `git status` — the arc-runner's / harness's own files (same as slices
  001–014), left alone.
