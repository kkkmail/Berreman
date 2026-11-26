# Step 0273 — state of the world

## Where we are

Spec 0050 section S8.3 (naming template). Step 268 gave the template grammar, 269 reading and writing names, 270 the
lab-label parser, 271 `setupNameOf` / `setupNameAmong`, and 272 named the window's new setups by the kit's template.
This step adds the kit-bar field and Save template, so a kit's naming template is edited in the window and written
to its kit file through two semantic messages. Step 276 names the grid's setups the same way.

## What's working

- The kit bar shows `Naming template:` with a field holding the kit's template, and Save template beside it while the kit takes writes
- `SetNamingTemplateText` and `SaveNamingTemplate` write the typed template into the kit file through `KitContext.saveKitFile`
- A text that does not parse, the kit's own template and a read-only kit are each refused and told, with nothing written
- The field takes the kit file's template on open (`initWith`, `loadKit`) and on Reload, so another writer's template shows
- New ids `NamingTemplateField` / `SaveNamingTemplateButton`; two KitWindowModelTests facts and one KitWindowTests fact added

## Tests

All run locally on the final tree. None exceeded its effective deadline.

- build: pass (full build 72 s, final incremental 15 s; deadline 600 s). 0 warnings from our code.
- constructor-unit-tests: pass, 1448 succeeded (baseline 1446, +2: `Save template writes the typed naming template into
  the kit file`, `a naming template that does not parse, or the kit's own, is told and writes nothing`). 69 s against
  a 180 s deadline. The changed `a kit opened read-only refuses every write message and writes nothing` and `Reload
  reads the kit's files in and clears the offer` pass. The new and changed facts were red first (they did not compile
  without the production symbols). The Reload fact was also shown red by behaviour: with the template line dropped
  from `withKitContent` it failed `Expected "{obj1}"`, actual the standard template.
- ui-smoke: pass, 245 succeeded (217 s, elevation running at the same time; deadline 600 s).
- ui-tests over `KitWindowTests`, `ScrollExtentTests`: pass, 29 succeeded (26 s; deadline 360 s). This includes the new
  `the kit bar shows the kit's naming template with Save template beside it`, the changed `a read-only window withholds
  every write control`, and `ScrollExtentTests`' kit-window fact unchanged.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` (204 s; deadline 600 s).
- invariant-s27-* (all fifteen): pass.

```yaml
gates:
  constructor_unit_tests: 1448
  ui_smoke_tests: 245
  ui_tests: 29
```

## Architecture

- `namingTemplateText` is set in `initWith` and in `withKitContent`, the one seam through which the open's `loadKit` and
  `ReloadKitChanges` take a kit file, so it is set in one place. `CopyChosenExperiments` re-shows the kit through the
  same seam ("as a reload sets it", step 187), so after a copy the field shows the kit's saved template.
- `SaveNamingTemplate` runs behind `guardKitWrite`, as `SaveSetup` does. It parses with `NamingTemplate.tryCreate`, refuses
  a template equal to `model.kit.kitFile.namingTemplate`, and otherwise writes `{ kitFile with namingTemplate = … }`
  through `saveKitFile`, whose returned context (with `modified` stamped) becomes `model.kit`. No setup or experiment
  is renamed. The window's own write records its stamp, so a requested check offers nothing.
- The row is a `DockPanel`, never a horizontal stack (AGENTS.md): the label docks left, Save template docks right in a
  wrapping `Border` (the `KitArchiveJob` Cancel precedent, since `verbButton` returns a finished `IView`), and the
  `TextBox` fills the rest, sending `SetNamingTemplateText` on every text change.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `OpticalConstructor.Tests` files reach the `[<AutoOpen>]` `ConstructorUiIdOfDomain` extensions (`NamingTemplate.caption`)
  only through `open OpticalConstructor.Ui`. `KitWindowModelTests` now opens it, as `SetupModelTests` does.
- The field has a 6 px right margin so Save template starts strictly right of it. The UI fact compares window
  coordinates (`TranslatePoint`) because the button's own `Bounds` are relative to its wrapping `Border`.
- The field stays editable in a read-only kit (only Save template is withheld). Typing changes the model only, and
  `SaveNamingTemplate` is refused by the guard.
- The new UI fact also types `{obj1}` and presses Save template headless, checking the kit status and the kit file on
  disk, so the view's wiring is covered as well as the model's.

## Changelog

- 2026-11-10 — Step 0273: the kit bar shows and saves the kit's naming template (`SetNamingTemplateText`,
  `SaveNamingTemplate` through `KitContext.saveKitFile`), refusing a bad, unchanged or read-only save; ids
  `NamingTemplateField` / `SaveNamingTemplateButton`; constructor-unit-tests 1446 → 1448.
