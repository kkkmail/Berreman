# Impl plan — spec 0033, slice 016 (ADD_COMPONENT UICOMP_XDUO_0002 SampleLibraryControls)

## Approach

Declare the samples-workbench list surface `SampleLibraryControls` in
`OpticalConstructor.Controls`, copying the step-015 `MaterialsControls`
shape (itself the LibraryControls.fs:25,34,64,76 conventions): pure
`Row` / `FacetOption` / `State` records, a `Handlers` record of
unit-returning functions, one `[<RequireQualifiedAccess>] module UiIds`
of `[<Literal>]` intent-named ids, a pure `selectedRow` helper, and a
domain-free `view` projection. The control renders host-flattened rows
and never touches `SampleProxy`; `State` mirrors the domain
`SampleQuery` seam (text + `SubstrateKind option` facet) as
host-supplied `code` strings (the host includes its own "all" option
that maps back to `substrate = None` at wiring time).

Differences from step 015, per the slice letter:

- ONE facet selector (substrate kind), not two.
- An extra creation verb: the multilayer entry point
  (`MakeMultilayerButton`, handler `makeMultilayer : unit -> unit`) —
  always enabled, like Add, since it starts a new-multilayer flow and
  needs no selection.
- No view-only/editability tier: the slice letter mandates none and the
  domain `Sample` record carries no "no edit model" state, so `Row` is
  `{ sampleId; label }` and Edit is simply disabled without a selection
  (like Remove / View), never removed.

TDD: add `SampleLibraryControlsTests.fs` + its fsproj entry FIRST,
capture the red build (FS0039 on the missing module), then implement
the production file and go green without touching the tests.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/SampleLibraryControls.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
  — compile entry after MaterialsControls.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleLibraryControlsTests.fs`
  (new) — 3 pure contract tests (`ui-tests`) + 2 headless structure
  proofs (`ui-smoke`): (a) mount over a known State, every UiIds
  control present, Add click dispatches ONLY the add handler, then each
  other verb / a row / a substrate option dispatches its matching stub;
  (b) with no selection the row-targeted verbs (Edit / Remove / View)
  dispatch nothing while Add and Make-multilayer stay live.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — compile entry after MaterialsControlsTests.fs.

## Risks

- FuncUI recycling forbids renaming styled controls in
  variable-membership child lists — use the AutomationId attr for rows,
  facet options, and verb buttons (the step-015 decision) and `Name`
  only on fixed structural controls.
- Keep LF discipline (`git diff --numstat` vs `--ignore-cr-at-eol`).
- ADD_COMPONENT declares only: no parent view / ribbon wiring this
  round; the registry entry for UICOMP_XDUO_0002 is supervisor-maintained.
