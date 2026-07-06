# Impl plan — spec 0033, slice 014 (rename the Library bay to Selector)

## Approach

A label rename, no behaviour change: the Main-screen ribbon bay named
"Library" becomes "Selector", freeing the word Library for the samples
workbench. `BayNames.library` (value `"Library"`) becomes
`BayNames.selector` (value `"Selector"`); `BayNames.all` and the `mainBays`
row follow. The bay's behaviour — kind-constrained, confirm-gated binding
through `LibraryControls` and the read-only `LibraryProxy` — is untouched,
and those two keep their names (per the slice letter).

TDD (rename flavour): update the test references first —
`BayNames.library` → `BayNames.selector` in `LibraryControlsTests.fs` and
`ExperimentControlsTests.fs`, strengthen the ribbon-membership test to pin
the literal label `"Selector"` and assert no bay is labelled `"Library"`,
and strengthen the existing headless render test to assert the
`RibbonTab_Selector` visual exists and no `RibbonTab_Library` does (the
acceptance's "verified by the headless UI tests"). The build then fails
FS0039 on the missing `selector` (red); the production rename makes it
green with no further test edits.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
  — `BayNames.library` → `BayNames.selector` = `"Selector"` (:150), the
  `all` list (:156), the `mainBays` row (now :1626 — the spec's :1582
  drifted), the Details-bay hint string that names the bay by label
  (:1615), and the comments in this file that call it "the Library bay".
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryControlsTests.fs`
  — the four `BayNames.library` references, test names / comments that
  mean the bay label, + the strengthened label assertions.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
  — the seven-bay order assertion (:74).

Out of scope (kept): `LibraryControls`, `LibraryProxy`, their UiIds
(`LibraryEntry_*`, `LibraryTree`, …), the `Library` domain module, the
`TreeLabel "Library"` grouping-tree root in `ElementId.fs` (library data,
not the bay), and comments in `OpticalConstructor.Controls` (untouched
project; the slice `touches` only TestWindows + Ui.Tests).

## Risks

- Stale references to the literal `"Library"` bay name outside the three
  files — grepped: none remain (the other `"Library"` hits are the
  grouping-tree label and doc comments in the Controls project).
- `count_at_least` gates: no test added or removed, counts stay at the 013
  baselines (119 / 373 / 54 / 249) — equal counts pass.
