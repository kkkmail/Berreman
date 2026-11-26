# Step 0310 — impl plan

The kit window attaches a picked file through the column-mapping wizard, by semantic messages (spec 0050 S9.2; 001 §P
`AttachDataFile`, §K).

## Approach

In `OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. `KitWindowShell` gains `openImportWizard : ImportWizardRequest -> unit`; `inert` holds `ignore`. Shells built as
   `{ KitWindowShell.inert with … }` (App `Program.fs`, the test shells) are unchanged.
2. `AttachDataFileTo of ExperimentId * FileName` becomes `AttachDataFile of ExperimentId * FileName * ImportProfileName`.
   Its arm records the message's profile (rows `AllDataRows`) instead of the detector's built-in; everything else stays.
3. New messages, behind `guardKitWrite`:
   - `OpenImportWizard of ExperimentId * FileName`: for a held experiment, hands the shell an `ImportWizardRequest`
     (the file, `experimentData.readPickedFile`, the kit's profiles, the detector's built-in as the default) and
     answers the model unchanged; an unknown id changes nothing.
   - `SaveImportProfile of ImportProfile`: `kitFile.withImportProfile`, then `kit.saveKitFile`, with the statuses the
     slice names (the `SaveNamingTemplate` shape).
4. `type ImportWizardApplied with member this.messages`: `SaveImportProfile p` (when `savedOpt = Some p`), then
   `AttachDataFile (experimentId, file, profile)`.
5. `attachDataFileButton` sends `OpenImportWizard` on a confirmed pick.

Tests:

- `KitWindowModelTests`: three new facts over a folder kit holding `E1` (committed through an `initInverse` scene,
  a polarizer R1 scan over 0…360°) with a shell recording wizard requests; the read-only fact also sends the three new
  messages and asserts the shell recorded nothing.
- `InverseConstructorTests`: `AttachDataFileTo` → `AttachDataFile … intensitySeries` (or `ellipsometricSeries`), and the
  ellipsometric fact is renamed.

## Risks

- The phone-ROI fact goes through the real file-backed proxy and `readMapped` with a kit profile: the scan column must
  validate against E1's R1 range (0 and 45 inside 0…360°).
- ELEV: new message cases are two-word names, unique; `messages` is a member on our type; no primitives in signatures.
- `git grep AttachDataFileTo -- Berreman/` must find nothing, including in comments.
