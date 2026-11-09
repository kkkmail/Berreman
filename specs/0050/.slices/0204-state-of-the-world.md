# Step 0204 — state of the world (IMPLEMENT: a kit's lab file read whole, as bytes)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits).

- Steps 196–202 built the send side; step 203 landed the copy's pure halves (`Experiment.renumberedFor` and
  `KitLibrary.referenceClosure`).
- This step gives the copy its byte-exact read: `KitProxy.readLabFile` answers any file of a kit, by its
  kit-relative path, as a `LabFileSource` (its absolute name in the kit and its bytes as stored). The copy carries
  those bytes to the other kit's `data/`; `readDataFile`'s decoded text drops a byte-order mark and cannot hold a
  binary file.
- The copy's IO and its UI belong to later S6 steps.

## What's working

- Add `KitProxy.readLabFile`: a kit file's absolute name and stored bytes by its kit-relative path, or
  `MissingKitFileErr` naming the path; `readOnlyView` passes it through.
- In memory, name the file under `C:/in-memory-kit/` and answer the stored text's UTF-8 bytes; `readDataFile` now
  reads through the same body.
- On disk, join the path under the kit folder with the platform separator and read the bytes whole (missing →
  `MissingKitFileErr`, IO exception → `KitIoErr`); `readDataFile` now reads through the same body.
- Add facts `a lab file is read whole by its kit-relative path, with its absolute name in the kit` (in-memory and
  on-disk) and `a lab file's bytes are read as stored, a byte-order mark included` (constructor unit tests
  1261 → 1264).

## Tests

These are development runs, recorded in `0204-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1264 passed, 0 failed, 0 skipped (1261 → 1264, +3). All three new test identities are
  `Passed` in the TRX.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1264
```

## Architecture

- **One read body per `KitProxy` implementation.** `readLabFile` is the raw read; `readDataFile` is `readLabFile`
  followed by `FileByteContent.kitDataFile` (decode, drop a leading byte-order mark, hash the stored bytes). So
  `ContentHash.ofBytes` of `readLabFile`'s content equals `readDataFile`'s hash by construction, in both proxies.
- **The answer reuses `LabFileSource`** (step 132), the same record an attach takes. A copy can hand a read file
  straight to the target kit's `attachLabFile` / `copyInFile`.
- **Absolute names follow each proxy's folder form.** On disk the platform separator (as `readDataFile` and
  `copyInFile` already joined); in memory `/` under `kitFolder` (step 133).

## Deferred

Nothing in this step's scope.

- The copy itself (reading the source kit's experiments, setups and data files, writing them renumbered with
  re-recorded hashes) and its UI are later S6 steps, by the spec's plan.

## Gotchas

- **The on-disk test adapter's `absoluteName` now replaces `/` with the platform separator** before
  `Path.Combine`. Step 133's form kept `/` on Windows and could never equal the proxy's platform name. Existing uses
  go through `tryKitPathOf` or open the file, so no assertion changed.
- **The in-memory `readLabFile` names the file by the requested path's spelling**, not the case-insensitively
  held one, matching `readDataFile`'s `path`.
- **`readLabFile` reads any kit file**, `.kit-json` and `.oc/lock` included; it is a read, not a policy. The copy
  decides which files to carry (S6, 001 §A: `.oc` is never sent).

## Changelog

- 2026-11-09 — step 0204: `KitProxy.readLabFile` in both implementations and `readOnlyView`; `readDataFile` reads
  through it; two new facts (three test identities).
