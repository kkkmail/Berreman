# Step 0012 — IMPLEMENT — impl log

## Progress

- [x] MaterialsControls.fs: delete the view-only gating
- [x] MaterialsControlsTests.fs: migrate the view-only fact
- [x] Build, elevation and tests (local observations)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/MaterialsControls.fs`:
  - `MaterialEditability` (`Editable` / `ViewOnly`) is deleted, and so is
    `MaterialsControlsRow.editability`.
  - The module doc's view-only sentences are deleted.
  - `verbRow` always lists Add / Edit / Remove / View. Edit, Remove and View share
    `onSelection`, which is offered on a listed selection and withheld otherwise. The `editVerb`
    list splice is gone, and the children are one literal list.
  - `verbRow`'s doc clause on Edit is deleted.
  - `verbButton` drops `View.withKey` and its comment, because the row's membership no longer
    changes. It now returns `Border.create [...] :> IView` directly.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsControlsTests.fs`:
  - `knownRows` lose `editability`, and their doc loses the view-only row.
  - `matchesId`'s doc loses the Edit-removal example.
  - `knownState`'s doc and the first ui-smoke fact's Edit comment lose their "editable" wording.
  - The fact `a view-only selection carries no Edit affordance — the button is removed, not
    greyed` becomes `every selected row offers the Edit verb and a click dispatches
    editMaterial`.
    - With silicon selected, the Edit verb must be present.
    - A click on it must leave the recorder holding exactly `[ "edit" ]`.

## Decisions

- **The migrated fact asserts the whole recorder, `[ "edit" ]`.** It does not just check that
  the list contains `"edit"`. That shows the click dispatched `editMaterial` and no other
  handler, which is the same strength as the first fact's per-verb counts.
- **`clickBox` keeps `View.withKey`.** The slice removes the key only from `verbButton`. The rows
  and facet options still change membership whenever the host's filter changes, so their keying
  still holds.

## Testing state

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- **build**: `dotnet build Berreman.slnx -c Release` exited 0 with 0 errors. The only warnings
  were the two vendored MathNet SYSLIB0051 ones that were already there.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s), 128s`. The projects were
  Controls, Ui, App, TestWindows and TestWindows.App.
- **ui-tests**: 3/3 succeeded under the MaterialsControlsTests filter (not-trait ui-smoke /
  kit-acceptance).
- **ui-smoke**: 208/208 succeeded on the whole suite, the same count as step 11. The TRX shows
  `every selected row offers the Edit verb and a click dispatches editMaterial` = Passed, and no
  view-only fact.
- **All five MaterialsControlsTests facts**: 5/5 succeeded.
- **The four s02 invariants** (`elev-list-empty`, `no-vstest`, `no-kit-kit-json` and
  `no-eps-axis-evaluated`) each find nothing.
- **LF line endings**: `git diff --numstat` equals `--ignore-cr-at-eol`, so there is no CRLF
  churn.

## Artifacts

- `.artifacts/0012-build.log`
- `.artifacts/0012-elevation.log`
- `.artifacts/0012-ui-tests.log`
- `.artifacts/0012-ui-smoke.log` and `.artifacts/0012-ui-smoke-results/ui-smoke.trx`
- `.artifacts/0012-materials-controls-tests.log`

## Gotchas

- **The Materials window keeps its own Edit gate.** `MaterialsWindowModel.editableSelection`
  reads `MaterialComplexity.isEditable`, is outside this step's effects, and is unchanged.
- **The ui-tests count under this step's filter is 3, not step 11's 14.** The two numbers come
  from different class filters (MaterialsControlsTests here, FsxAcceptanceTests there), so they
  are not a regression.
