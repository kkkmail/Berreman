# Step 0201 — state of the world (IMPLEMENT: Send kit… as semantic messages of the kit window)

## Where we are

Spec 0050, section S6 (send and receive kits).

- Steps 196–200 built the send rule, the `KitArchiveProxy` seam with its double and the real zip writer, `sendKit` /
  `KitZipPicker`, and the Ui job host and job state.
- This step composes them into the kit window's model:
  - the shell carries the zip picker, the archive and the job host;
  - the model carries the window's one send;
  - four semantic messages start a send, follow its per-file progress, end it and cancel it.
- Step 202 adds the visible Send kit… verb and its progress row to the kit bar. Step 233 composes the real archive and
  host into the windows.

## What's working

- Add `zipPicker`, `archive` and `archiveJobs` to `KitWindowShell`. `inert` holds the none picker, an in-memory archive
  and the inert host.
- Add `kitSend : KitArchiveJobState` to the model, idle at `initWith`.
- Add `SendKit`, `SendKitProgressed`, `SendKitEnded` and `CancelSendKit`. A send runs through the shell's job host:
  one per window, refused for a scratch kit, allowed for a read-only kit.
- Build every full shell as an update of `KitWindowShell.inert`: the App's mounted shell and the test shells.
- Add six `KitWindowModelTests` facts over real folder kits and the real zip writer (constructor-unit-tests
  1253 → 1259).

## Tests

These are development runs, recorded in `0201-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors. The only warnings are the vendored MathNet `SYSLIB0051` ones already on the start tree.
- **constructor-unit-tests:** 1259 passed, 0 failed. All six new identities pass.
- **ui-smoke:** 236 passed.
- **ui-tests** (`KitWindowTests`): 6 passed.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 projects.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1259
  ui_smoke_tests: 236
```

The ui-tests count (6) is only the selected class, so it is not a suite baseline.

## Architecture

- **The shell carries the IO, and the model carries the state.**
  - `KitWindowShell` holds the three seams a send reaches: the picker, the archive and the job host.
  - The model holds only `KitArchiveJobState`.
  - The `SendKit` arm partially applies `archive.sendKit kitFile folder zip` and hands it to
    `archiveJobs.startArchiveJob`. The host supplies the observer and posts `SendKitProgressed` / `SendKitEnded` back.
- **A send is not a kit write.** It reads the kit folder and writes a zip outside it, so it is not behind
  `guardKitWrite`. A kit opened read-only is sent unchanged, which is how a reviewer returns a kit (001 §E).
- **One send per window.** A `SendKit` while one runs answers the model unchanged. No queue or retry exists.
- **The view file qualifies `JobRunner.` and `KitArchiveJob.` rather than opening them.** This avoids shadowing by
  their single-word cases and record-label capture by `KitArchiveProgressIds`.

## Deferred

Nothing in this step's scope.

- The Send kit… verb, its save dialog and its progress row in the kit bar are step 202's, by the spec's own plan.
- Composing the real archive and the `onUiThread` host into the windows is step 233's.

## Gotchas

- **The `immediate` host runs the whole send inside `SendKit`.** Its recorded messages must then pass through the
  update in order, and only then is the window idle again.
- **The real `writeKitZip` does not create the zip's folder.** A save dialog always answers an existing folder, so the
  facts create `exports` first.
- **`KitWindowShell.inert` builds a fresh record, and a fresh in-memory disk, on every access.**
  `[<ReferenceEquality>]` means two of them never compare equal, as before.
- **A test that needs the fact folder as well as the kit folder uses `withFactFolder`.** `withTitledFolder` is
  `withFactFolder` with the fact folder dropped.

## Changelog

- 2026-11-09 — step 0201:
  - the kit window's Send kit messages, `kitSend` and the shell's picker, archive and job host;
  - the App shell and the test shells are built as updates of `inert`;
  - six `KitWindowModelTests` facts.
