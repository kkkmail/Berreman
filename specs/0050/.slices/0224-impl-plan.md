# Step 0224 — IMPLEMENT — impl plan

## Approach

Spec 0050 S6's first copy operation: `KitContext.copyExperimentsFrom`, in
`OpticalConstructor.Storage/KitContentFiles.fs`, beside the typed experiment reads and writes it uses (steps 146-149).

- `ExperimentCopyReport = { experiments; dataFiles; snapshots }` is declared in `module KitContentFiles`.
- `type KitContext with member this.copyExperimentsFrom (source) (ids)` returns
  `Result<ExperimentCopyReport, KitLibraryError>`.
  - It reads everything before it writes anything, in the brief's order:
    - (a) the access check;
    - (b) the source's experiments, keeping the chosen ones in source order; a missing id is refused;
    - (c) the source's snapshots and the `referenceClosure` over them, minus the versions the target holds;
    - (d) each distinct data path read through `source.proxy.readLabFile`;
    - (e) renumbering over the target's ids.
  - It then writes, in order: the snapshots, the data files (`copyInFile` into the path's data folder, else
    `data`), and the experiments with their new attachments.
  - It reports the experiments, data files and snapshots as written.
- All helpers stay local to the member. No module-level function is added, so ELEV013 and ELEV015 cannot fire.
- The member uses no tuple of repeated types.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`: the report type and the member.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCopyTests.fs` is new: `module ExperimentCopyTests`
  with the eight declared facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: the new file is appended
  last to the compile list, with no comment.

## Risks

- **The target's exact `paths`.** The brief lists `.kit-json`, `data`, `data/raw`, `data/raw/run 1.csv`, `library`
  and the snapshots. A read-write open also holds `.oc` / `.oc/lock`, and the copy writes `E1.experiment-json`.
  The fact asserts the full, real list and the impl-log records why.
- **Latest-snapshot resolution.** It duplicates a private three-line pattern in `KitLibrary.fs`
  (`resolveOverlayReference`'s `readSnapshot`). That file is outside this step's authorized edits, so the resolve
  stays local to the member.
- **The ELEV gate** runs from the arc origin over all projects. The new code must be ELEV-clean: no new public
  free function, unique names, no primitive in a signature.
