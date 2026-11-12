# Step 0312 — impl plan (WIRE_UI)

## Approach

Step 311 built `ImportWizardWindow` and the shell member `KitWindowShell.withImportWizard owner dispatch`. This step
composes that member into the App and proves the whole path end to end through a real folder kit.

1. `OpticalConstructor.App/Program.fs`
   - Open `OpticalConstructor.Ui.ImportWizardOpening`.
   - In `ConstructorScene.mount`, follow the shell it builds with `.withImportWizard window (fun msg -> hostDispatch.Value msg)`.
     The lambda defers the read: `hostDispatch` holds `ignore` until the init command stores the Elmish dispatch.
   - Add one clause to `mount`'s doc saying that Attach data file… opens the wizard over this window (spec 0050 S9.2).
2. `OpticalConstructor.Ui.Tests/KitWindowTests.fs`
   - Open `OpticalConstructor.Ui.ImportWizardOpening`.
   - Add the ui-smoke fact `acceptance: Attach data file maps a picked lab file through the wizard into the kit and validates it`:
     - a fresh folder kit opened read-write through `openKitAt` (`openedAt`), loaded into the inverse seed
       (`kitWindowModel … InverseConstructor`);
     - `E1` recorded through the model as step 310's fixture records it (linear polarizer, `ExpChooseElement`, `ExpCommit`),
       with its one R1 axis asserted;
     - a shown headless host `Window`, and a model held in a ref and dispatched through `updateTableAndElementRotation`;
     - the shell `KitWindowShell.inert.withImportWizard host dispatch`;
     - the Phone ROI file (BOM followed by the UTF-8 text) written under the class's test directory, outside the kit;
     - `OpenImportWizard`, then pressing the `Phone ROI CSV` choice and Attach at their centres;
     - assertions on the closed wizard, E1's mapped attachment, the copied bytes, the kit file read back and the badge
       text `validated (2 points)`.
3. `AGENTS.md`: one bullet (at most six lines) after 'An experiment is a scan over a setup revision'.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `AGENTS.md`

## Risks

- The inverse seed over the AppContext's stores must still let `ExpCommit` record `E1`. The forward kit-window fact
  already commits over a linear polarizer with these stores.
- Attach closes the wizard on pointer-press, so the press helper must not release on a closed window.
  `pressAtCentre` already has that guard.
- A curried member's `Window` parameter, given a `HostWindow`: the compiler may need an explicit upcast.
