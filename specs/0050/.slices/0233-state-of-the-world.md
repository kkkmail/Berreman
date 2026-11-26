# Step 0233 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0233 (WIRE_UI), obligation
`S6-KIT-SHELL-WIRE`.

- Steps 145, 150 and 197–201 built the seams: the kit folder dialog, the archive, the job host, the zip dialogs and
  the progress row.
- Steps 225–232 put Send kit… and Copy experiments… on the kit bar and wired Open kit from zip… end to end.
- This step gives the constructor windows' kit bar the real seams in place of the inert shell, so Send kit… and Copy
  experiments… work in the app.

## What's working

- `AppContext` carries `locations : DataLocations`, set by `compose`, so `create` and `recreateLibrary` both hold the
  locations they were composed over.
- `ConstructorScene.mount` takes the `AppContext`, and its kit shell holds four real seams:
  - the Send kit… save dialog over the exports folder;
  - the real `KitArchiveProxy`;
  - UI-thread archive jobs that dispatch `SendKitProgressed` / `SendKitEnded`;
  - the Copy experiments… kit folder dialog over the kits folder.
- `MainConstructorWindow` and `InverseConstructorWindow` pass their context to `mount`. Their docs say the kit bar
  sends and copies through the real dialogs.
- Added the ui-tests fact `the app scope carries the locations it was composed over`.
- Added the ui-smoke fact `a constructor window's kit bar offers Send kit, and Copy experiments only to a kit opened
  read-write`.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests and ui-smoke over everything discovered;
- ui-tests over `AppContextTests`, `KitWindowTests`, `LauncherTests` and `TableAndElementRotationTests`;
- the fourteen s22 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0233-*`:

- **build:** 0 errors. The only warnings are the vendored MathNet's two `SYSLIB0051`, the same as step 0232.
- **constructor-unit-tests:** 1353 passed, unchanged against checkpoint 231.
- **ui-smoke:** 242 passed. That is 241 + 1, the declared fact.
  - It took 201 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **ui-tests** over the four selected classes: 63 passed.
  - By class: AppContextTests 17 (16 + 1, the declared fact), KitWindowTests 13, LauncherTests 8,
    TableAndElementRotationTests 25.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects.
- **The fourteen s22 invariant greps:** none finds anything.

No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1353
  ui_smoke_tests: 242
  ui_tests: 63
```

## Architecture

- **The app scope owns the locations; the window owns the dialogs' owner.**
  - `AppContext.locations` is the one record the composition root composed over, so a window that holds only the
    context still knows where its dialogs start: exports for Send kit…, kits for Copy experiments…
    (001 §G; .spec-md §3.9).
  - `mount` applies both dialogs to its own window.
- **No new seam, message or model field.** `KitWindowShell`, `KitZipPicker.ofWindow`, `KitArchiveProxy.create`,
  `KitArchiveJobHost.onUiThread` and `KitFolderPicker.ofWindow` already existed. This step only composes them. The
  windows' public constructors are unchanged.
- **The job host posts through the mount's captured dispatch.** That is the same `hostDispatch` the activation check
  uses. It is read when a job reports, after `init` has stored it.

## Deferred

Nothing in this step's scope is deferred. The constructor windows' Send kit… and Copy experiments… are wired to the
real seams.

## Gotchas

- **`hostDispatch` is filled by `init`, after the shell is built.** Every shell callback must dereference it when
  called, never when the shell is built.
- **The kit bar smoke fact cannot tell a real shell from an inert one.** Neither button is pressed, because a real
  window's dialogs are the system's (the S5.3 precedent), and the two verbs' visibility depends only on the kit
  folder, the running send and the kit's writes. The shell's composition is pinned by `ConstructorScene.mount`
  itself.
- **The shell still starts from `KitWindowShell.inert`**, as the slice prescribes. Every current field is overridden,
  but a field added to the shell later starts inert until a step wires it.
- **`LauncherWindow` keeps its own `locations` argument.** It needs them even when the library fails to open and
  there is no `AppContext`: for the Data locations link and for Recreate library.

## Changelog

- 2026-11-09 — Step 0233:
  - added `AppContext.locations`;
  - wired the constructor windows' kit shell to the real Send kit… save dialog (exports folder), archive, UI-thread
    job host and Copy experiments… kit dialog (kits folder);
  - added one ui-tests fact (AppContextTests 16 → 17) and one ui-smoke fact (241 → 242).
