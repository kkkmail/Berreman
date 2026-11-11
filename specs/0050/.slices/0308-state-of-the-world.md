# Step 0308 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 307 added the kit's import profiles, the one read of a lab file through any profile, attach-with-profile,
  the copy of profiles between kits, and the sniff and preview of a picked file.
- This step adds the wizard's pure MVU in `OpticalConstructor.Ui/ImportWizardView.fs`. It opens on a picked file, shows
  how its layout reads, and takes a built-in or kit profile or a new column mapping. On Attach, and only then, it hands
  the kit window the profile to attach by and any new profile to save.
- Next:
  - step 309 adds the wizard's view and ids;
  - step 310 has the kit window open it and act on `ImportWizardApplied`;
  - step 311 adds its window.

## What's working

- Add `ImportWizardView`, the column-mapping wizard's pure MVU (001 §K; spec 0050 S9.2). It holds:
  - the request, the applied choice and the context;
  - the profile choice;
  - the model with its `preview`;
  - eight semantic messages.
- The wizard opens on the picked file's one read. It takes the sniffed layout, chooses the detector's profile, and
  holds one unmapped column per preview column.
- Picking a profile, changing the layout, mapping a column or naming the mapping changes only the wizard. A kit
  profile's pick also takes its layout.
- Attach hands the kit window one `ImportWizardApplied` and then closes. A new mapping must pass the name rules and
  `ColumnRoles.tryOfChoices`; otherwise its refusal is held in `lastError` and nothing is applied. An unread file can
  only be cancelled.
- Add `ImportWizardTests`, six facts over a recording context.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Error(s)`; no `error` in stdout | 76 s for the first build, whose 2 warnings are the vendored MathNet `SYSLIB0051`; 16 s for the final incremental build, with `0 Warning(s)` | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 194 s | 600 s |
| constructor-unit-tests | Passed!, 1522 of 1522, 0 failed, 0 skipped | 75 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 227 s, then 217 s over the final binaries | 600 s |
| ui-tests (`ImportWizardTests`) | Passed!, 6 of 6, 0 failed, 0 skipped | 2 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` (`section_invariants --find-nothing`) | under 1 s each | 300 s |

- **ui-tests: 6.** These are the selected class `OpticalConstructor.Ui.Tests.ImportWizardTests` under the gate's exact
  command (`--filter-not-trait` ui-smoke and kit-acceptance, plus the class selector). The count rises by 6, as the
  slice says. In the TRX, all six identities are `Passed` under the slice's exact names:
  - `the wizard opens on the picked file with its sniffed layout and the detector's profile chosen`
  - `picking a profile, changing the layout or mapping a column changes nothing outside the wizard`
  - `Attach with a picked profile hands it to the kit window once and closes`
  - `Attach with a new mapping hands the kit window the profile to save and attaches by it`
  - `a mapping that cannot be a profile is told and nothing is applied`
  - `a picked file that cannot be read offers only Cancel, and Cancel applies nothing`
- **constructor-unit-tests: 1522** and **ui-smoke: 251**, each equal to its latest checkpoint (steps 306 and 305). No
  constructor or smoke test changed.
- **Elevation** analyzed all 12 product projects (a standalone run with no comparison base), including Ui and its
  dependents App, TestWindows and TestWindows.App.
- **Red by construction.** Every symbol the six facts call is new at this step, in the new module `ImportWizardView`.
- **Mutation check: ten mutations, all killed.** `.artifacts/0308-mutation-checks.py` / `.log`. The mutations were:
  - the initializer ignores the sniff;
  - a relayout forgets the held roles;
  - a kit profile's pick keeps the mark;
  - mapping a column keeps the picked profile;
  - Attach applies over an unread file;
  - a new mapping is attached without its profile to save;
  - Attach closes before `onApplied`;
  - the name is not checked against the kit;
  - Cancel does not close;
  - an unknown profile's pick is taken.

  The source's SHA-256 was verified restored, and the tree was rebuilt before the final ui-tests and ui-smoke runs.

```yaml
gates:
  constructor_unit_tests: 1522
  ui_smoke_tests: 251
  ui_tests: 6
```

## Architecture

- **The wizard has the shape of step 107's import dialog.**
  - A `[<ReferenceEquality>]` request and context carry the injected read and the kit window's callbacks.
  - The model holds the context and is independent of any window.
  - `updateImportWizard` is the only transition. `ApplyImportWizard` over a read file and `CancelImportWizard` are the
    only messages that reach the context: `onApplied` first, then `requestClose`. Every other message changes the
    model alone (.spec-md §10.3: nothing is attached or saved until Attach).
- **One read, held as a `Result`.**
  - The initializer calls `request.readPickedFile request.file` exactly once and keeps the `Result` as `source`.
  - `preview` is computed from the held bytes through `KitFileText.previewWith` (step 307), so a layout change never
    reads the disk again.
  - An unread file previews as `MalformedDataFileErr` of the read's `describe`.
- **Reuse, no new rules.** The wizard adds no parser, no name rule and no role rule. It calls:
  - `LabFileSource.sniffedLayout` and `previewWith` (step 307);
  - `ChosenImportProfile.tryOfName` and `ColumnRoles.tryOfChoices` (step 300);
  - `ImportProfileName.availableAmong` (step 299) and `ImportProfileName.tryCreate` (step 291).
- **One column rule.** The private `wizardColumnsOver` remakes the columns over a preview: numbered from 1, a held
  column keeps its role, a new one is unmapped, and a failed preview has none. The initializer, the two layout
  messages and a kit profile's pick all use it.
- **`initImportWizard` is a member of `ImportWizardContext`** (`context.initImportWizard`), because of ELEV013. See
  Gotchas.
- Every new shape is transient UI state. No stored format, schema, version, migration or package changed.

## Deferred

Nothing. Every requirement of the slice landed this round. The wizard's view (step 309), the kit window's messages
(step 310) and its window (step 311) are later steps' slices, not deferrals.

## Gotchas

- **Spec deviation, ELEV013: `initImportWizard` is a member of `ImportWizardContext`, not a free function.**
  - The slice writes `initImportWizard (context : ImportWizardContext) : ImportWizardModel`.
  - A free function of exactly one of our types is an ELEV013 finding. The analyzer exempts only a module named for
    the type, and `ImportWizardView` is not. AGENTS.md says the analyzers win.
  - Step 107 met the same rule for `initImportDialog` and made it `ImportDialogContext.initImportDialog`
    (`0107-impl-log.md`). This step follows that precedent.
  - The behaviour is exactly the slice's. Steps 310 and 311 call `context.initImportWizard`.
- **A kit profile's pick remakes the columns.** It changes the layout, so it remakes the columns by the same rule as
  `SetImportDelimiter` / `SetImportHeader`. That keeps "one choice per preview column" true for a later
  `StartColumnMapping`. The slice says only that the pick sets the layout. Over the fixture, `Lab scan`'s layout gives
  the same five columns, so no fact depends on this choice.
- **"Inert" means the model is returned unchanged**, `lastError` included. That covers a pick of an unknown profile,
  `SetColumnRole` on a column the model lacks, and `ApplyImportWizard` over an unread file. Every other message clears
  `lastError`.
- **Attach's error order is the slice's:**
  1. a blank name (`BlankProfileNameErr "the profile name is empty"`);
  2. `availableAmong` (a built-in's name, then a kit profile's);
  3. `ColumnRoles.tryOfChoices`.

  So the "two signal columns" case needs a free name to reach the role rule.
- **Record-label overlap.** `ImportWizardModel` has `context`, `delimiter`, `header` and `bom` labels, which
  `ImportProfile`, `LabFileSniff` and the dialog models share. Annotate literals of those types: the module and its
  tests do.
- **The facts are pure MVU.** They need no Avalonia session, so they are plain `[<Fact>]`s, as step 107's model facts
  are.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0308: the column-mapping import wizard's pure MVU (S9.2).
  - New `ImportWizardView`:
    - `ImportWizardRequest`, `ImportWizardApplied`, `ImportWizardContext`, `WizardProfileChoice`;
    - `ImportWizardModel` (with `preview`) and `ImportWizardMsg`;
    - `ImportWizardContext.initImportWizard` and `updateImportWizard`.
  - New `ImportWizardTests`, six facts.
