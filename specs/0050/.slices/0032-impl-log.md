# Step 0032 — ADD_CONTRACT STORE_BigBoy_0001 CatalogueProxy — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt, the slice, its gates, 001 §A and §B, .spec-md
  §3 to §5, `LibraryEntryFiles.fs`, `SourceProxy.createInMemory`, and the later slices that consume the seam
  (33, 34, 35, 38, 39).
- [x] The four facts in `CatalogueProxyTests.fs`, written before the production file. No red run was observed,
  because this round runs no checks.
- [x] `CatalogueFiles.fs`: `CatalogueIds`, `CatalogueError`, `CatalogueData`, `CatalogueProxy`, `createInMemory`.
- [x] The two project-file lines, with no new comment.
- [x] Read-only checks (`.artifacts/0032-tree-checks.log`).
- [x] The contract registry needs no edit: `.contracts-json` already holds `STORE_BigBoy_0001` as `declared` with
  `declaringStep` 32, and `.contract-ids/BigBoy-json` holds its identity. The contract stays `declared`; step 33
  implements it.
- [x] Attempt 2, after the code judge's route-back (`.artifacts/code_judge/0032-01-code-judge.md`): the fixtures in
  `CatalogueProxyTests.fs` no longer read any code seed, and the Gotchas that said later work moves them are replaced.
  `CatalogueFiles.fs`, the project files, the hash literals and the four facts did not change.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueFiles.fs` (new): the seam, in `module CatalogueFiles`.
  - `CatalogueIds.builtInCatalogue`, shaped like `MaterialLibrary.MaterialIds`.
  - `CatalogueError`: `CatalogueIoErr`, `CatalogueFileRejectedErr`, `MissingEntryFileErr`, `UnlistedEntryFileErr` and
    `ManifestMismatchErr`, each `of reason : string`, then `InvalidCatalogueEntriesErr of LibraryEntryError`.
  - `CatalogueData` with `tryCreate` and `validate`.
  - `[<ReferenceEquality>] CatalogueProxy` with exactly `readCatalogue` and `writeCatalogue`, and its intrinsic
    `createInMemory`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `Compile` line directly
  after `LibraryEntryFiles.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueProxyTests.fs` (new): `module CatalogueProxyTests`,
  with facts (a) to (d) under the slice's exact names, over test-local fixtures (attempt 2).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `Compile` line directly
  after `EntryFileJsonTests.fs`.

## Decisions

- **`tryCreate` orders the envelopes by looking up each manifest row.** It uses `List.choose` with `List.tryFind`, not
  `List.find`, so no path can throw. A row always finds its envelope, because the rows are built from those envelopes
  and `CatalogueManifest.tryCreate` has already refused a repeated id + version. This keeps the sort rule in step 26's
  one place, as the slice asks.
- **`validate` tests two conditions, and neither implies the other.** The rebuilt manifest is sorted, so it equals the
  stored one even when the envelopes are reversed; only the row-order comparison sees that. A replaced row hash is seen
  by the manifest comparison.
- **Fact (c) also asserts that the reason names the catalogue** (`'Built-in'`), because the slice says
  `ManifestMismatchErr` names it. This stays inside the slice's four facts.
- **Fact (a) asserts order on what was read back**, not only on the `tryCreate` result, and against the literal
  material, sample, source order. A source-first input can only produce that order through the manifest's rows.
- **No `describe` on `CatalogueError`.** Step 39 adds the error render with its own fact; adding it here would go
  beyond this slice.
- **The fixtures are test-local records (attempt 2).** They are keyed to `MaterialIds.langasite`, `CategoryIds.crystal`,
  `SampleIds.multilayerQwId` and `PresetIds.tiSapphire800`, which step 58 keeps.
  - The material follows the `materialWith` / `epsOnly` shape at `EntryFileJsonTests.fs:57-66`. Its constant uniaxial
    indices are arbitrary fixture values; no fact reads them.
  - The sample uses `starterMultilayerStructure`, so it is one period and not the 41-layer seed. The fixture is named
    `quarterWaveCell` to say so.
  - The source is `ProtectedBuiltIn`, which `ofSourcePreset` requires under `BuiltInProvenance`.
  - `langasite` and `tiSapphire800` keep their names, so no fact body changed, including fact (d)'s "Langasite again".
- **Fact (a)'s order is unaffected.** The rows sort by `kind.value` first (material 1, sample 2, source 3), and the three
  entries are still one of each kind, so the new names cannot move them.

## Testing state

**This round compiled and ran nothing, and it ran no gate.** `add_contract_worker.system-md` says "you act; you run no
checks" and "never run, self-report, or green-light a gate". So neither attempt ran `dotnet build`, `dotnet test`, the
ELEV script or an invariant command.

What the gate engine has measured so far (`.manifest-arc-runner.log`):

- `build` passed on attempt 1's tree at 20:12:42Z. That was before attempt 2's fixture edit, so **the edited test file
  has never been compiled**.
- `elevation` and `constructor-unit-tests` have not run on any attempt, so **the four facts have never executed**.

Attempt 2's read-only checks of `CatalogueProxyTests.fs`, recorded here and not in the log file:

- It contains none of `SeedSamples`, `SeedSources`, `seedEntries`, `seedTrees`, `builtInEntries` or `standardCategories`,
  nor the rest of step 58's acceptance grep (`seedExperiments`, `allSeedSamples`, `allSeedSources`, `createInMemoryEmpty`).
- It has 0 CR bytes, a final LF and no BOM.
- It holds exactly 4 `[<Fact>]`s under the slice's names, and 4 distinct hash literals of 64 lowercase hex characters.
- `git status` under `Berreman/` still shows only the four authorized paths, and `CatalogueFiles.fs` was not edited.
- Every symbol the new fixtures use was read at its definition: `WaveLength.nm` (`Fields.fs:296`), the `nm` measure
  (`Constants.fs:19`), `RefractionIndex` (`MaterialProperties.fs:35`), `UniaxialTransparent` and `EpsWithoutDispValue`
  (`Dispersion.fs:552`, `:749`), and the `MaterialEntry`, `Sample` and `SourcePreset` fields. The four added `open`s are
  ones `EntryFileJsonTests.fs` already uses.
- Step 58's edit list omits this file. Only slices 0032, 0039 and 0092 may edit it, read from each slice's `effects`.

What attempt 1 checked by reading and read-only commands (`.artifacts/0032-tree-checks.log`):

- All four fact names match the slice's test ids exactly, and the file holds exactly 4 `[<Fact>]`s.
- All four hash literals are 64 lowercase hex characters and distinct.
- Both `Compile` lines sit where the slice puts them.
- `CatalogueProxy` declares exactly `readCatalogue` and `writeCatalogue`.
- Every changed or new file has 0 CR bytes, a final LF and no BOM. `git diff --numstat` equals `--ignore-cr-at-eol`.
- `git status` shows only the four authorized paths under `Berreman/`, and nothing under `Elevation/` or
  `.elevation-analyzers.json`.
- The new type, error-case and module names occur nowhere else in Berreman's `.fs` files or in STL's. The only other
  `builtInCatalogue`s are `private` values in two test files, which the analyzers do not read.
- `Error (InvalidCatalogueEntriesErr error)` was read against `NoCallSiteRewrap.fs`: ELEV001 reports only a single-case
  union wrapping another elevated type's `.value`, so this line is not its shape. `NoPrimitiveBesideElevated.fs` was read
  for ELEV010: the new signatures carry only `unit` beside our types, as `SourceProxy` does.
- The tests use the existing builders and `Assert` overloads the same way `LibraryEntryFilesTests.fs` does.

Those checks are reasoning, not measurement. A compile error, a failing fact or an ELEV finding is still possible.

**Expected count, not measured:** `constructor-unit-tests` should reach 780, which is the arc-runner's checkpoint of 776
plus the 4 new facts.

Every requirement of the slice is addressed: (1) to (5) and facts (a) to (d). Exit `commit_ready: true` means exactly
that. It does not mean the gates passed: `gates_run` and `gate_results` are empty.

## Artifacts

- `specs/0050/.artifacts/0032-tree-checks.log`: attempt 1's read-only checks. It was captured just before the test file's
  unused `open OpticalConstructor.Domain` was removed; the end of the log re-counts CR bytes after that edit. It predates
  attempt 2's fixture edit.

## Gotchas

- **The prompts disagree on local checks, and workers in this arc have gone both ways.** "You run no checks"
  (`add_contract_worker.system-md`) conflicts with "Run the relevant tests after every change" (AGENTS.md), with the
  project prompt's testing procedure and with the base prompt's test-driven section.
  - This round followed the role prompt. It is the instruction specific to this role, and the base prompt calls local
    gate runs "the legacy single-worker self-check path".
  - Steps 26 to 31 took the opposite reading and ran the build, tests and ELEV as development feedback. Step 31 recorded
    that a strict reading would count that as a breach. Those earlier runs do not authorize this one.
  - The cost of this reading: a compile error or ELEV finding here reaches the gate engine rather than being caught in
    the round. The judge's route-back already spent one slot (`count=1/2`), so one more failure on this slice exhausts
    the budget.
  - An operator ruling would settle this for the remaining steps.
- **`contentHash` is a label on both `CatalogueManifestRow` and `LibraryEntryEnvelope`.** `{ row with contentHash = … }`
  needs `row`'s type known, so fact (c)'s lambda annotates it.
- **`CatalogueProxyTests.fs` must not read a code seed, because nothing later can repair it.** Step 58 deletes
  `builtInEntries`, `SeedSources` and `SeedSamples`, its acceptance grep must find those names nowhere under `Berreman/`,
  and its edit list omits this file. Only slices 0032, 0039 and 0092 may edit the file; 39 adds a render fact and 92 comes
  after 58. So the fixtures are test-local records over the ids step 58 keeps. Attempt 1 read the seeds and said step 58's
  work would move these fixtures; slice 0058 says the opposite, and that claim is withdrawn. For the same reason this
  file's comments do not name the deleted seeds, since the grep would match a comment too.
- **`grep -c $'\r'` can give a false count in Git Bash here.** It reported every line of these three notes as holding a
  CR while `tr`, Python and `git ls-files --eol` (`w/lf`) all showed none. The cause was not traced. Attempt 2's 0-CR
  results above were re-measured from raw bytes. Count bytes, or use `git ls-files --eol`.
