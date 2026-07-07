# Impl log — spec 0033, slice 016 (ADD_COMPONENT UICOMP_XDUO_0002 SampleLibraryControls)

## Progress

- [x] Read task file, worker system prompt (add_component_worker + the
      arc-runner base), project prompt, slice spec, slice-015
      impl-log/SoW, MaterialsControls.fs, LibraryControls.fs,
      MaterialsControlsTests.fs, TestApp.fs, the two fsprojs, and the
      domain SampleQuery / SubstrateKind seam (ElementId.fs).
- [x] Red: SampleLibraryControlsTests.fs + its fsproj entry added first;
      the build fails with 120 × FS0039 naming the missing
      `SampleLibraryControls` (`016-red-tdd.log`).
- [x] Implementation: SampleLibraryControls.fs (new, ~250 lines) + the
      Controls fsproj entry.
- [x] Green: all five gates pass in the advisory local runs on the FIRST
      attempt after red — no test edits after red.
- [x] Contract registry: `specs/0033/.contracts-json` already records
      `UICOMP_XDUO_0002` as `declared` / `declaringStep: 16`
      (supervisor-maintained) — nothing for the worker to write.
- [x] LF policy verified (see Gotchas).
- [x] State-of-the-world written.

## Decisions

1. **Shape copied from `MaterialsControls` (the step-015 rendering of the
   LibraryControls.fs:25,34,64,76 conventions), ids as `[<Literal>]`s.**
   Pure `Row` / `FacetOption` / `State` records, a `Handlers` record of
   unit-returning functions, one `[<RequireQualifiedAccess>] module UiIds`,
   the pure `selectedRow` helper, and the domain-free `view` projection.
   The eight slice-mandated ids are `[<Literal>]`s; the derived per-row /
   per-facet-option ids are prefix functions (`SampleRow_` /
   `SampleSubstrateOption_`), the `LibraryControls.UiIds.entry` precedent.
2. **No editability tier — `Row` is `{ sampleId; label }`.** Unlike step
   015 (whose slice letter mandated the view-only Edit-removal for the
   `complexity = None` built-ins), this slice letter names no view-only
   behaviour and the domain `Sample` record carries no "no edit model"
   state — every seeded sample has a `structure` and a `substrate`. Edit
   therefore behaves like Remove / View: present always, disabled while
   nothing (listed) is selected. Recorded here because it is a deliberate
   divergence from step 015's shape, not an omission.
3. **`MakeMultilayerButton` is a second creation entry point.** "The
   multilayer entry point" starts the make-a-multilayer flow (the
   `Repeated` / `PeriodGroup` shape of `SampleStructure`), so like Add it
   needs no selection and is ALWAYS enabled; Add keeps the accent as the
   primary verb. The handler is `makeMultilayer : unit -> unit` — what the
   entry point opens (builder dialog, template flow) is the host's
   business at wiring time.
4. **`FacetOption` is defined locally, not reused from
   `MaterialsControls`.** The sibling controls are deliberately
   self-contained modules (LibraryControls / ExperimentControls /
   MaterialsControls each own their row types); importing another bay's
   record type would couple the two components' evolution for a two-field
   record. Same-shape, separate types — the established Controls idiom.
5. **Domain-free state mirrors the `SampleQuery` seam.** `searchText` +
   `selectedSubstrate` echo the domain query's text + `SubstrateKind
   option` facet as host-supplied `code` strings (the host includes its
   own "all" option and maps it back to `substrate = None` at wiring
   time), so `OpticalConstructor.Controls` never references Domain. The
   control renders the rows it is given — no filtering here; the live
   `TextBox.onTextChanged` idiom dispatches `setSearchText` so the HOST
   re-runs its search seam.
6. **AutomationId (not `Control.Name`) on every control in a
   variable-membership child list** — the rows (the host's filter
   rewrites them), the substrate options (a host-supplied list), and the
   five verb buttons (kept consistent with step 015, where the idiom is
   load-bearing; here the verb-row membership is fixed today, but the
   AutomationId keeps it safe if a later slice adds a conditional verb).
   Fixed structural controls (the search TextBox, the substrate
   WrapPanel, the rows StackPanel) keep `Name`. Tests match by Name OR
   AutomationId (the `matchesId` idiom).
7. **Component declared, not wired** (ADD_COMPONENT obligation): no
   parent view, TestWindows host, or Ribbon bay references
   SampleLibraryControls this round; the headless tests mount the
   component alone in a plain Window.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/SampleLibraryControls.fs`
  (new) — the component: `Row`, `FacetOption`, `State` + `empty`,
  `Handlers` (incl. `makeMultilayer`), `UiIds`, the pure `selectedRow`,
  and the `view` projection (search row, the substrate facet selector,
  the rows list, the five-verb row), styled to MATCH the sibling bars'
  idle/chosen palette.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
  — `<Compile Include="SampleLibraryControls.fs" />` after
  MaterialsControls.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleLibraryControlsTests.fs`
  (new) — 3 pure contract tests (`ui-tests` gate): empty state, the
  stable intent-named ids + derived prefixes, `selectedRow` resolution;
  2 headless structure tests (`ui-smoke` trait): (a) mount over the
  known State, every UiIds control present, Add click dispatches ONLY
  the add handler, then Edit / Remove / View / Make-multilayer / a row /
  a substrate option each dispatch their matching stub handler; (b) with
  no selection the row-targeted verbs are present but inert (disabled —
  clicks dispatch nothing) while Add and Make-multilayer stay live.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — `<Compile Include="SampleLibraryControlsTests.fs" />` after
  MaterialsControlsTests.fs.

## Testing state

TDD: red first (120 × FS0039 naming the missing `SampleLibraryControls`,
`016-red-tdd.log`), then the production module, then green on the first
run with no test edits after red. All five gates in the slice roster pass
in the worker's local ADVISORY runs (per Invariant 6 the arc-runner gate
engine is the sole gate authority and re-runs them after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors, no
  warnings from the new files (the NU1901/2/4 CoreWCF advisories are
  pre-existing and untouched).
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; this slice
  adds no Domain code).
- `ui-smoke` — 58 passed, 0 failed (56 baseline + the 2 new headless
  structure tests).
- `ui-tests` — 255 passed, 0 failed (252 baseline + the 3 new pure
  contract tests).

Acceptance check (inside the ui-smoke run): the headless test mounts
SampleLibraryControls over a known State, finds every UiIds control, and
a simulated Add click invokes the add handler (and no other verb
handler).

## Artifacts

- `specs/0033/.artifacts/016-red-tdd.log` — red build (FS0039 × 120).
- `specs/0033/.artifacts/016-build.log` — solution build.
- `specs/0033/.artifacts/016-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/016-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/016-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/016-ui-tests.log` — ui view tests.

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does
  not exist (same as slice 015); the file lives at
  `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\multistep\add_component_worker.system-md`
  and was read from there.
- No `## Operator note` content is in flight for this attempt (the
  project prompt's heading is present but empty).
- **Invariant 6 vs the base protocol's "run gates locally":** the
  add_component_worker delta says the worker runs no gates; the arc's
  established contract-family precedent (slices 003 / 015) still runs
  them as ADVISORY local verification because the SoW's structural gate
  requires the `gates:` baseline YAML counts. Followed the precedent;
  the counts reported here are advisory only.
- **Ambiguity resolved: no view-only tier for samples** (Decision 2) —
  if a later slice introduces non-editable built-in samples, extend
  `Row` with an editability DU THEN (the MaterialsControls pattern is
  ready to copy); do not pre-build it here.
- **Ambiguity resolved: Make-multilayer needs no selection** (Decision
  3) — it is a creation flow, not a transform of the selected row. If
  the wiring step decides it seeds from the selection, the host can read
  its own `selectedId`; the control's contract does not change.
- **Line endings:** the two edited fsprojs show ZERO CRLF churn
  (`git diff --numstat` identical with and without `--ignore-cr-at-eol`).
  The two NEW files are LF on disk, matching the committed siblings and
  the `.gitattributes` (`*.fs text eol=lf`) policy.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder
  show in `git status` — the arc-runner's / harness's own files (same as
  slices 001–015), left alone.
