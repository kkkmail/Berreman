# Step 0266 — implementation plan

## Approach

The source slot's item is the run's source: it emits its own Stokes state times its intensity scale (spec 0050 S8.2;
001 §N "The source is real"). Until now every chart read step 252's default emission.

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. **`model.runEmission : Result<ExperimentRun.SourceEmission, ControlCaption>`**, on the model extension that holds
   `tryItemModelChoice` and `resolveSlotItem` (steps 263, 265). It reads the first `LightSource` slot holding an item:
   - `model.resolveSlotItem` gives the item and its model, or `UnknownSlotItemErr` naming it (step 255). That error's
     `describe` is the caption.
   - A resolved item emits through `choice.emissionWith (ParameterValues.ofParameters item.parameters)` (step 246).
     An `ItemEvaluationError` answers its `describe`.
   - With no such slot, it answers `SourceEmission.ofValues (ParameterValues.defaultsOf SourcePreset.lightSourceAxes)`,
     step 252's default.
   - Reusing `resolveSlotItem` keeps the two `UnknownSlotItemErr` texts in one place ("Reuse, never copy").
2. **`chartForRun`** reads `model.runEmission` where it read the default emission. An `Error text` shows step 252's
   `The run cannot be evaluated` chart with that text. The wavelength stays `boundSource`'s, because `withItem` binds a
   source item's slot to its preset (step 258). The docs say so, citing S8.2.

`OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`: two facts over `initMain ()`. The source slot `src` holds a
`Laser` item of the HeNe preset (`AddItem`, `AssignSlot`). The model version comes from step 210's shipped entry,
`BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId`. One linear polarizer, pre-bound to its preset,
stands before the seeded intensity detector and is swept over R1 0 … 180° in 7 points.

- `a source item emits its own polarization through the run`:
  - with the item's defaults, each S₀ is 1, as with no item;
  - with `degreeOfPolarization` 1 and `azimuth` 30°, each S₀ is 1 + cos 2(x − 30°) within 1e-12;
  - `runEmission` is the item's `emissionWith` result.
- `a source item whose parameters the run cannot evaluate shows that the run cannot be evaluated`:
  - with `degreeOfPolarization` 1.5, the chart is titled `The run cannot be evaluated`, and its description is
    `ParameterOutOfRangeErr`'s `describe`, naming `degreeOfPolarization`;
  - a source item the kit no longer holds shows `UnknownSlotItemErr`'s text.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`

## Risks

- **Charts with no source item must not change.** `runEmission` then answers exactly the default emission that
  `chartForRun` built before, so every existing chart is unchanged. That covers `FsxAcceptanceTests`, the chart classes
  and export.
- **`initMain`'s source slot is unbound.** The run therefore reads 600 nm until the HeNe item binds it through
  `withItem`. The ideal polarizer does not depend on λ, so the assertions do not either.
- **`SetParameterState` must accept the out-of-range value 1.5.** `Parameter.validate` checks only finiteness, so it
  does. The range check is the evaluator's `checkedFor` (step 245).
