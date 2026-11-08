# Implementation log — 0156

## Progress

- Read the task, worker base and WIRE_UI delta, project prompt, AGENTS.md, naming conventions, slice and resolved gate snapshot, and scratch-kit ruling `.manual/010`.
- Inspected the current launcher, kit-opening and import-dialog composition and existing launcher test helpers. `pendingDecisions` already includes every snapshot row; `withRebindings` owns overlay reconstruction.
- Next: author the three authorized headless facts before changing the launcher wiring, then connect the existing components.
- Authored exactly the three declared smoke facts and shared fixture helpers in `LauncherTests.fs`. They write canonical snapshots through the kit proxy, drive the real dialog, record every import-proxy mutation, compare held entries/logs, and verify Cancel and title-bar close release locks. The first attempt used an invalid Materials-browser membership assertion for the overlay; retry 02 replaces it as recorded below. The named opener acceptance exercises both a missing user material and an all-Reuse Silicon snapshot, including Cancel/reopen/Apply.
- Before launcher wiring: extract the current error rendering into a local helper, retain the locked-kit offer, and add the pending-dialog ownership transition with one Apply flag. Preserve the final-composition tests unchanged.
- Wired `Program.fs`: only an empty `pendingDecisions` list bypasses the dialog. Apply calls `withRebindings` and passes the resolved kit to Main/Inverse. Overlay errors close the kit and reuse the existing diagnostic. The dialog's Closed handler closes the kit unless its Apply callback has already taken ownership. Both access modes use this same path.
- Reviewed the source against `S43-RESOLVE-ON-OPEN` in the immutable execution contract. Preserved the locked-kit offer, empty-snapshot behavior and all existing tests. No new message, ID, serializer, durable format or import behavior was introduced.
- Source diff line counts match `--ignore-cr-at-eol`; there is no line-ending churn in either changed source file. The pre-existing supervisor manifest edit was not changed by this worker. Final-composition source remains untouched.
- Completed the state-of-the-world at the supplied path, carrying forward checkpoint 154's measured baseline without treating the three authored facts as passed.
- Retry 02, before repair: read the judge's finding and traced step 0142's overlay contract. `MaterialProxy.overlaidWith` changes per-entry reads but preserves `listMaterials`; the Materials node assertion is invalid for the intentionally missing user material. Replace that proof with canonical decoded-entry comparisons through the actual stores supplied to the launched Main constructor model. Add a read-only composition property in the already-authorized `Program.fs`; preserve the launcher lifecycle and every other acceptance obligation.
- Retry 02, after repair: `MainConstructorWindow.sessionLibraryStores` exposes the same store record passed to `initMainWithKit`, without reconstructing an overlay in the test. After clicking Apply on the real dialog, `confirmSnapshotFromLauncher` compares both `tryGetMaterial` and `resolveVersion` against `snapshot.content.toMaterialEntry`, and asserts the material listing/search functions are the original app functions. The missing database entry, visible rows, Cancel/reopen/Apply, lock ownership, no-write observations, explicit all-Reuse invocation and all three declared test identities remain. The invalid browser assertion and its extra window opening are removed; no listing behavior changed.
- Retry 02 updated the plan and state-of-the-world to describe per-entry composition proof. No build, test, analyzer, invariant or census command was run. Gate execution and measured-count publication remain the supervisor's responsibility.

## Files modified

- `specs/0050/.slices/0156-impl-plan.md`
- `specs/0050/.slices/0156-impl-log.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `specs/0050/.slices/0156-state-of-the-world.md`

## Testing state

No checks run. WIRE_UI Invariant 6 reserves build, elevation, constructor-unit-tests, ui-smoke, selected LauncherTests ui-tests and section invariants for the supervisor after worker exit. Test identity count must rise by exactly three smoke facts; no census or baseline edits are authorized.

No build/test/analyzer/invariant result is claimed for this repair. All requested implementation is authored; `commit_ready` remains false pending supervisor verification. Inherited checkpoint 154 records constructor_unit_tests = 1149 and ui_smoke_tests = 221. The snapshot fixtures observe import writes as well as database state so an idempotent write cannot pass unnoticed. Real launcher/dialog input and visible decision rows supply the semantic-tree assertion; the resulting Main window's actual scene stores supply the per-entry overlay proof.

## Artifacts

Worker reports are written at the supplied slice paths. Any persistent execution artifacts belong under `specs/0050/.artifacts/`.

## Gotchas

- `.manifest.state.json` was already modified on entry and is supervisor-owned; leave it untouched.
- The three authorized new facts are the only added identities. Preserve every existing assertion and the final-composition launcher.
- No published spec or durable format changes are needed.
- The two all-Reuse obligations share the same real-window assertion helper; the named opener acceptance itself includes Silicon, cancellation, reopening and explicit Apply.
- The Apply flag is set before rebuilding the overlay so its error path closes once and the subsequent dialog Closed event does not release a successfully opened constructor's kit.
- Step 0142 deliberately preserves material listings and searches. A snapshot-only material resolves by id or version but is absent from the Materials browser; never change that listing contract to satisfy an overlay-composition assertion.
