# Step 0224 — IMPLEMENT — impl log

## Progress

- [x] `ExperimentCopyReport` and `KitContext.copyExperimentsFrom` in `KitContentFiles.fs`.
- [x] `ExperimentCopyTests` (the 8 declared facts), appended last to the test project's compile list.
- [x] Local build / tests / mutants / elevation / invariants (development runs; the gate engine gives the verdict).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`, appended after
  `KitLibraryContext.recordItem`:
  - `ExperimentCopyReport = { experiments : Experiment list; dataFiles : DataAttachment list; snapshots : LibraryVersionRef list }`,
    with a two-line doc citing spec 0050 S6.
  - `type KitContext with member this.copyExperimentsFrom (source : KitContext) (ids : ExperimentId list) :
    Result<ExperimentCopyReport, KitLibraryError>`, with a three-line doc citing spec 0050 S6. Its helpers are all
    local to the member:
    - `keep` — (b) the source's experiments whose id `ids` names, in source order. The first named id the source
      lacks is `KitLibraryKitErr (MissingKitFileErr "E<n>.experiment-json")`.
    - `lackedSnapshots` — (c). It runs `referenceClosure` over the kept experiments' `libraryReferences`, resolving
      each reference against `source.proxy.listSnapshots ()`:
      - an exact version resolves to that snapshot;
      - a latest reference resolves to the entry's highest-version snapshot;
      - a reference with no snapshot is `UnheldEntryErr reference.describe`.

      It then drops every version `this.proxy.listSnapshots ()` holds.
    - `readDataFiles` — (d) each distinct `dataFileOpt` path, in order of first appearance, read through
      `source.proxy.readLabFile`.
    - `renumber` — (e) `Experiment.renumberedFor` over the ids of `this.proxy.readExperiments ()`.
    - `writeSnapshots`, `copyDataFiles`, `writeExperiments` and `writeAll` — (f), in the brief's order: snapshots,
      then data files, then experiments.
      - Each data file goes through `copyInFile <folder> lab.file.leafName lab.content`. `<folder>` is the path's
        `parentOpt` when `KitDataFolder.tryCreate` accepts it, else `KitDataFolder.root`.
      - Each experiment's `dataFileOpt` is replaced by the new attachment of its source path.
    - A `KitError` is mapped to `KitLibraryKitErr`. The first failure stops the copy and keeps what it wrote.
  - A read-only target answers `KitLibraryKitErr (KitReadOnlyErr "copyExperimentsFrom")` before any read.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCopyTests.fs` (new): `module ExperimentCopyTests`
  with the eight declared facts.
  - Its kits are `KitProxy.createInMemory` over fresh `InMemoryKitFolder`s. Each is initialised with a test-local
    `KitFile` and opened through `KitContext.openKit` with a local stepping clock.
  - The source is filled read-write before it is closed and reopened read-only:
    - its lab files are placed by `placeLabFile` and attached in place through `readLabFile` → `attachLabFile`;
    - its experiments are recorded through `{ kit; imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy }.recordExperiment`.
  - The experiments are those the brief names:
    - E1 binds `SampleIds.multilayerQwId` v1 and `PresetIds.source600` by entry id, and attaches `data/raw/run 1.csv`;
    - E2 binds `SampleIds.glassPlate1mmId` v1 and attaches nothing;
    - E3 binds nothing and attaches `data/b.csv`;
    - E5, used only by the shared-file fact, attaches E1's file.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`:
  `<Compile Include="ExperimentCopyTests.fs" />` is appended last to the compile list, with no comment.

## Decisions

- **The first fact's exact `paths`.** The brief lists `.kit-json`, `data`, `data/raw`, `data/raw/run 1.csv`,
  `library` and the closure's `library/<name>`s. The real tree also holds:
  - `.oc` and `.oc/lock`, from the target's read-write open (step 125's in-memory lock), not from the copy;
  - `E1.experiment-json`, which the copy writes and which the same fact reads back through `readExperiments`.

  The fact asserts the full, real list in ordinal order, which is stronger than leaving those paths out. It also
  asserts that the target lists exactly E1's closure as snapshots.
- **E1's closure is spelled out** as catalogue envelopes in visit order: source preset, multilayer, vacuum material,
  vacuum category, glass 1.52, glass category. This is the order `KitLibraryTests.recordingClosure` already
  establishes. It is not derived through `referenceClosure`, so the fact does not mirror the implementation.
- **The latest-snapshot resolution is local to the member.** `KitLibrary.fs`'s private `resolveOverlayReference` /
  `readSnapshot` filters and sorts the same way, but that file is outside this step's authorized edits. The brief
  places the operation in `KitContentFiles.fs`. No shared helper was extracted, so no edit outside the effects was
  needed.
- **No module-level function** was added. Every helper is local to the member, so ELEV013 and ELEV015 cannot fire.
  Tuples pair two different types (`KitRelativePath * LabFileSource`, `KitRelativePath * DataAttachment`).
- **Beyond the brief, the facts also check:**
  - the read-only fact checks that the source is not read at all: a watching proxy records `listKindFiles`,
    `readKindFile`, `readLabFile` and `listSnapshots`;
  - the refusal facts compare the target's whole held content: paths, every path's read, snapshots and experiments;
  - the missing-id fact also tries `[E1; E9]`, so a held id before the missing one does not slip a write through.

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6). Logs are in
`specs/0050/.artifacts/0224/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - First build: one error, mine. The test file needed `open OpticalConstructor.Storage.CatalogueImportStore` for
    `CatalogueImportContext.create`. The production projects compiled. That build's only warnings were the two
    existing vendored MathNet `SYSLIB0051`.
  - Then exit 0, **0 warnings, 0 errors** (`build.log`).
  - Final rebuild after the mutants were restored: exit 0, **0 warnings, 0 errors** (`build-final.log`). The
    Storage dll is newer than the restored source.
- **constructor-unit-tests** (all discovered): **1331** passed, 0 failed, 0 skipped.
  - That is 1323 (checkpoint 223) + 8.
  - All eight declared identities are `Passed` in the TRX (`constructor-unit-tests.trx`).
  - Re-run over the final build: 1331 passed (`constructor-unit-tests-final.log`).
- **elevation** (`ASG_ARC_BASE_COMMIT` = arc origin `f395597`, ELEV004 excluded, over the final build):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 201 s against its 600 s estimate (`elevation.log`);
  - the run covered all 12 projects because `Berreman.slnx` changed since the arc origin;
  - suppressed findings went from 1311 to 1312 (measured, not gated). The one new suppression is ELEV004, the
    `.value` that names the missing experiment's file in `MissingKitFileErr`: the error's reason is a string, so
    this is the boundary line.
- **The fourteen `invariant-s22-*` must-find-nothing greps:** each finds nothing (`invariants.log`). A second sweep
  with `--untracked` covers the new test file and also finds nothing.
- **The facts bite.** Each mutant was applied to `KitContentFiles.fs`, the test project rebuilt, and the class run.
  The source was then restored byte for byte, checked with `cmp`. The script and logs are in `.artifacts/0224/mutants/`.
  - **m1:** held snapshots not filtered fails `a snapshot the kit already holds…`.
  - **m2:** no `List.distinct` on data paths fails `a data file two copied experiments share…`.
  - **m3:** `dataFileOpt` not replaced fails `a copied data file whose name the kit holds…`.
  - **m4:** no renumbering fails `a copied experiment whose id the kit holds…`.
  - **m5:** snapshots written before the data files are read fails `a copy whose data file the source lacks writes
    nothing`, plus two report facts.
  - **m6:** data folder dropped (always `data`) fails the exact-paths fact, the suffix fact and the shared fact.
  - **m7:** no read-only guard fails `a kit opened read-only copies nothing`.
  - **m8:** a missing id ignored fails `an experiment the source does not hold…`.

## Artifacts

`specs/0050/.artifacts/0224/`:

- `build.log`, `build-final.log`;
- `experiment-copy-tests.log`;
- `constructor-unit-tests.log`, `constructor-unit-tests-final.log`, `constructor-unit-tests.trx`;
- `elevation.log`, `invariants.log`;
- `mutants/`: `run-mutants.sh`, the original source, and each mutant's diff, build log and test log.

## Gotchas

- `CatalogueImportContext.create` is a Storage extension (`OpticalConstructor.Storage.CatalogueImportStore`), not a
  Domain member. A test that records through the library opens both.
- Every `InMemoryKitFolder` claims the same absolute folder (`C:/in-memory-kit`). Two in-memory kits therefore see
  each other's absolute names as "inside". The copy uses `copyInFile` directly, as the brief says, never
  `attachLabFile`, so the target never mistakes a source file for its own.
- An in-memory read-write open adds `.oc` and `.oc/lock` to `paths ()`. A fact that asserts exact paths over a
  read-write kit lists them.
