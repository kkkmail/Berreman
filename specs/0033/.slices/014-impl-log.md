# Impl log — spec 0033, slice 014 (rename the Library bay to Selector)

## Progress

- [x] Read task file, worker system prompt (implement_worker + arc-runner
      base), project prompt, slice spec, slice-013 state-of-the-world /
      impl-log, TableAndElementRotationView.fs, Ribbon.fs,
      LibraryControlsTests.fs, ExperimentControlsTests.fs; grepped every
      `BayNames.library` / `"Library"` / `"Library bay"` reference in the
      solution.
- [x] Red: test references updated to `BayNames.selector` + the label
      assertions (literal `"Selector"`, no `"Library"` in `BayNames.all` /
      the mainBays names, `RibbonTab_Selector` present and
      `RibbonTab_Library` absent in the headless render); build fails with
      7 × FS0039 naming the missing `selector` (`014-red-tdd.log`).
- [x] Implementation: `BayNames.library` → `BayNames.selector` with value
      `"Selector"`, `BayNames.all`, the `mainBays` row, the Details-bay
      hint string, and the in-file comments that named the bay.
- [x] Green: all five gates pass in the advisory runs on the first attempt,
      no test edits after red (counts equal the 013 baselines — a pure
      rename adds no tests).
- [x] State-of-the-world written.

## Decisions

1. **Line-number drift resolved by symbol, not by line.** The slice cites
   `BayNames.library` at :150, `BayNames.all` at :156 and the mainBays row
   at :1582; in the current file the sites are :150 / :156 / :1626 (the
   file grew since the spec was written). The rename targets the symbols,
   which are unique — no ambiguity.
2. **The Details-bay hint string is included in the rename.** The
   `detailsState` fallback title said "pick one in the Library bay" — a
   user-facing reference to the bay by its old label. Renaming the label
   while leaving the hint pointing at a non-existent "Library bay" would
   contradict the acceptance's "no bay labelled Library" in spirit, so the
   hint now says "pick one in the Selector bay". "No Library entry bound"
   stays — the library *concept* (LibraryProxy, Library entries) keeps its
   name per the slice letter. No test asserts this string (grepped).
3. **Acceptance verified in the headless layer.** The acceptance demands
   "a Selector tab … and no bay labelled Library, verified by the headless
   UI tests". The existing ui-smoke render test now also asserts, on the
   rendered window, that a `RibbonTab_Selector` visual exists (via
   `Ribbon.UiIds.tab BayNames.selector`) and `RibbonTab_Library` does not;
   the pure ribbon test additionally pins the literal `"Selector"` and
   `Assert.DoesNotContain("Library", …)` over `BayNames.all` and the live
   `mainBays` names. Assertions were strengthened inside existing tests —
   no new facts, so the `count_at_least` baselines are unchanged.
4. **Scope held to the declared `touches`.** `LibraryControls`,
   `LibraryProxy`, their UiIds (`LibraryEntry_*`, `LibraryTree`,
   `LibraryBoundReadout`), the `Library` domain module, and the
   `TreeLabel "Library"` grouping-tree root in `ElementId.fs` (library
   DATA, not the bay) are untouched, as are the historical "Library bay"
   doc comments in the `OpticalConstructor.Controls` project (outside the
   slice's `touches` of TestWindows + Ui.Tests).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
  — `BayNames.selector = "Selector"` (with a doc note recording the
  rename), `BayNames.all`, the `mainBays` row, the Details-bay hint
  string, and four comments that named the bay ("Library bay" →
  "Selector bay": the model's `library` field doc, the `init` doc, the
  `RequestBindValueId` comment, the `libraryState` doc).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryControlsTests.fs`
  — the four `BayNames.library` references → `BayNames.selector`; the
  ribbon-membership test renamed and strengthened (literal `"Selector"`,
  no `"Library"` among the bay names); the headless render test renamed,
  selects `BayNames.selector`, and asserts `RibbonTab_Selector` present /
  `RibbonTab_Library` absent; comments updated where they meant the bay
  label (module doc records the rename).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
  — the seven-bay order assertion now lists `BayNames.selector`.

## Testing state

TDD: red first (7 × FS0039 naming the missing `selector`,
`014-red-tdd.log`), then the production rename, then green on the first
run — the tests were not modified after red. All five gates in the slice
roster pass in the worker's local ADVISORY runs (the arc-runner gate
engine re-runs them authoritatively after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors.
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline).
- `ui-smoke` — 54 passed, 0 failed (= baseline; includes the renamed
  Selector-bay confirm-gated render test with the new tab assertions).
- `ui-tests` — 249 passed, 0 failed (= baseline; includes the
  strengthened ribbon-membership and seven-bay order tests).

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol` for all
three touched files.

## Artifacts

- `specs/0033/.artifacts/014-red-tdd.log` — red build (FS0039 ×7).
- `specs/0033/.artifacts/014-build.log` — solution build.
- `specs/0033/.artifacts/014-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/014-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/014-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/014-ui-tests.log` — ui view tests.

## Gotchas

- **The spec's mainBays line number drifted** (:1582 in the slice letter,
  :1626 in the file) — the symbols are unique, so the rename went by
  symbol (Decision 1).
- **Only the LABEL moved.** `LibraryControls` / `LibraryProxy` / the
  `Library` domain module / `LibraryEntry_*` UiIds all keep their names by
  design — do not "finish the rename" in a later slice without a spec
  step; the slice letter explicitly frees only the word Library at the
  ribbon for the samples workbench.
- The `TreeLabel "Library"` in `ElementId.fs:511` is the seeded
  grouping-tree ROOT label (library data shown inside the Selector bay's
  tree), not the bay label — deliberately untouched.
- `.manifest.state.json` shows as modified in `git status` — the
  arc-runner's own file (same as slices 001–013), left alone.
