# Step 0273 — impl plan

Spec 0050 S8.3: the kit bar shows the kit's naming template in a field and saves it with Save template, through two
semantic messages (001 §J, §P).

## Approach

1. `ConstructorUiIdOfDomain.fs`: `type Kit.NamingTemplate with member this.caption : ControlCaption` (the template's
   text), beside `Kit.SetupName.caption`.
2. `UiIds.fs`, the kit bar group: `namingTemplateField` (`NamingTemplateField`) and `saveNamingTemplateButton`
   (`SaveNamingTemplateButton`).
3. `TableAndElementRotationView.fs`:
   - model field `namingTemplateText : ControlCaption`, set from the kit file in `initWith` and in `withKitContent`
     (the one seam both the open's `loadKit` and `ReloadKitChanges` take a kit file through);
   - messages `SetNamingTemplateText of ControlCaption` and `SaveNamingTemplate`;
   - arms: `SetNamingTemplateText` sets the field; `SaveNamingTemplate` runs under `guardKitWrite`, parses with
     `NamingTemplate.tryCreate`, refuses a bad text (`Not saved — <describe>`) and an unchanged template
     (`The naming template is unchanged.`), and otherwise writes through `model.kit.saveKitFile`
     (`Saved the naming template.` / `Not saved to the kit — <describe>`).
4. `KitWindowView.fs`: after the Kit menu's `WrapPanel`, a `DockPanel`: `Naming template:` docked left, `Save template`
   (a `verbButton` in a right-docked `Border`, the `KitArchiveJob` precedent) only while `model.kitWrites` is offered,
   and the `TextBox` fill carrying `namingTemplateField`, dispatching `SetNamingTemplateText` on every text change.
5. Tests: two new `KitWindowModelTests` facts, the read-only and Reload facts extended; one new `KitWindowTests` fact,
   the read-only window fact extended (no `saveNamingTemplateButton` in a read-only kit window, present in a
   read-write one).

## Risks

- `ScrollExtentTests`' kit-window fact: the bar gains one row; its tallest model is read-only, so only the label and
  field are added. Run it.
- The fill `TextBox` must end before the button starts: the field takes a right margin so the button's left is
  strictly beyond the field's right.
- `withKitContent` is also the copy's re-show; the copy re-shows the saved template as a reload does (recorded).
