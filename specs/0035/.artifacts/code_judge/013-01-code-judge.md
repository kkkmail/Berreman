# Code judge — 013.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\013.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\013-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\013-impl-log.md`
- Gate results: build=pass, unit-tests=pass, constructor-unit-tests=pass, ui-smoke=pass, ui-tests=pass
- Critic critiques: (none — no critics ran this cycle)

## Rationale

Every gate in the step-013 roster is `pass`, and the diff meets each stated
slice-spec requirement. The picker's `materialRow` (`SampleEditorView.fs`) now
stacks a search box (`materialSearchRow`) and a catalogue-driven category facet
(`materialCategoryRow`) above the option WrapPanel, and the options map over
`filteredMaterials m` rather than the raw `m.materials`. `filteredMaterials`
narrows the once-resolved list purely through the Domain search seam
(`byQuery { text; category; dispersion = AnyDispersion } { entries = m.materials }`)
— it does not re-call `MaterialProxy.listMaterials`/`searchMaterials`, honouring
the spec's "filter the resolved material list the window loads once" wording and
coding no new filter. The by-`MaterialId` selection contract
(`ChooseMaterial of MaterialId`) is untouched: the only change inside the option
map is the source list; each clickBox still dispatches `ChooseMaterial entry.id`.

The facet derivation is a faithful mirror of the Materials bay: I confirmed
against `OpticalConstructor.Ui/MaterialsView.fs:180-183` that both build the
facet as `"All"` plus `standardCategories |> List.filter (visibility =
SelectableOnCreate)` in catalogue order, so a newly seeded category flows through
without a code edit and `Vacuum` (`HiddenOnCreate`) correctly gets no facet
button. State is added as two elevated Model fields (`materialSearchText : string`,
`materialCategory : CategoryId option`, where `None` is the "All" facet — not a
magic string), each with a one-line `update` arm and an `init` seed, keeping the
view a pure projection. The three mandated stable UiIds are present and
intent-named (`SampleMaterialSearchBox`, `SampleMaterialCategoryFilter`, and the
derived `SampleMaterialCategory_<code>` keyed on the CategoryId's Guid).

New public surface is exercised by tests in the diff, satisfying the done-green
coverage criterion. The pure `ui-tests` fact drives `filteredMaterials` and the
new `SetMaterialSearchText`/`SelectMaterialCategory` messages through `update`,
asserting the fragment narrows to glasses (dropping the crystal), the Crystal
facet lists only Crystal-category entries, a filtered option still dispatches
`ChooseMaterial` with its `MaterialId`, and the "All" facet restores the full
list. The headless `ui-smoke` acceptance fact drives the real window by its
UiIds — types `"glass"`, clears and clicks the Crystal facet, then chooses the
filtered crystal and Add-layer, proving the `MaterialId` survived by asserting the
anisotropic-only `layerOrientation 0` editor renders for the added layer. This is
the exact clever proof the impl-log's Gotchas describes (a layer's material name
carries no UiId), and it holds up.

The SoW and impl-log line up with the diff on every material point: touched files,
new fields/messages/helpers, the reuse-not-reimplement filter decision, the
catalogue-driven facet, and the count deltas (ui-smoke 95→96, ui-tests 327→328;
unit-tests and constructor-unit-tests projects untouched). No critic raised a
finding — none ran — and I find no unmet requirement, layering violation, or
duplication of my own. The slice is green.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and the diff meets every slice-013 requirement: a search box (SampleMaterialSearchBox) and a catalogue-driven category facet (SampleMaterialCategoryFilter + derived SampleMaterialCategory_<code>) now narrow the once-resolved material list via filteredMaterials, which reuses the Domain byQuery seam over m.materials without re-calling MaterialProxy. The facet derivation mirrors MaterialsView.fs (\"All\" + standardCategories filtered to SelectableOnCreate). The ChooseMaterial of MaterialId selection contract is unchanged. New public surface (filteredMaterials, SetMaterialSearchText/SelectMaterialCategory, the new UiIds) is exercised by both a pure ui-tests contract fact and a headless ui-smoke acceptance fact in the diff, the latter proving the MaterialId survived via the anisotropic-only layerOrientation editor. SoW and impl-log match the diff.", "retry_hint": ""}
```
