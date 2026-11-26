# Step 0262 — impl plan

## Approach

Add `SaveSetup` to the constructor (spec 0050 S8.2): the table is written over the active setup by the one version
rule (`Setup.revisedWith` with `Setup.pinFor`), in place while no experiment references its revision and as the next
revision after; refused with no active setup, over an unchanged table, and under a read-only kit (step 185's
`guardKitWrite`). The setup row shows the active setup and gains a `Save setup` button.

1. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `TableAndElementRotationMsg.SaveSetup` with a one-line doc.
   - Model members shared by `ExpCommit`, `CreateSetup` and `SaveSetup` (reuse, never copy): the table's recorded
     slots, the active setup with the table written over it, and the model with a recorded setup in place of its
     name's (ordinal order) and active. The two existing arms call them instead of their inline copies.
   - The `SaveSetup` arm exactly as the brief: `No setup is active: create one first.`, `Setup '<name>' is
     unchanged.`, `Saved setup '<name>', revision <n>.`, `Not saved to the kit — <e.describe>`.
   - The view: a wrapping `TextBlock` (`activeSetupText`) first in the setup section, outside the horizontal row (the
     AGENTS.md wrapping rule); `Save setup` after `Create setup`, withheld with no active setup or a read-only kit.
2. `OpticalConstructor.Controls/UiIds.fs`: `activeSetupText` / `saveSetupButton` beside the setup-row ids.
3. Tests (census):
   - `SetupModelTests`: two new facts.
   - `KitWindowModelTests`: the read-only fact sends `SaveSetup` with a setup active (a `Bench` created over the
     table while the kit was read-write and activated in the read-only window before the draft is prepared).
   - `SetupSaveLoadTests`: the smoke fact renamed and extended (active-setup text with and without a setup, Save setup
     offered / withheld and dispatching `SaveSetup`); a new live-window fact drives Create setup, the Experiments
     bay's Add and Save setup, then reads both revisions back through `readSetups`.
   - `KitWindowTests`: `writeControlIds` gains `saveSetupButton`; `assertWriteControls` activates a `Bench` setup
     recorded while the kit was read-write, so the read-only window withholds Save setup for the kit's access, not
     for a missing setup, and the read-write window offers it.

## Risks

- `ScrollExtentTests` (every control scrollable on screen): the new text and button live in the existing vertical
  section; the row stays narrow.
- The live-window fact turns the polarizer between Add and Save; it dispatches the rotation through the dispatch
  the view receives (no extra message, field or id).
