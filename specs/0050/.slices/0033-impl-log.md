# Step 0033 — IMPLEMENT_CONTRACT STORE_BigBoy_0001 CatalogueProxy — impl log

## Progress

- [x] Read the protocol, the project prompt, AGENTS.md, the slice, 001 §A, `.spec-md` §3–§5, steps 26–32's code and notes,
  and the ELEV rule sources in STL (read-only).
- [x] `StorageError.describe` (`Errors.fs`): one line per case.
- [x] `CatalogueFolderStore.fs` with `LibraryFileText.writeAtomically` and `CatalogueProxy.create`, plus its `Compile` line
  directly after `EntryFileJson.fs`.
- [x] `CatalogueFolderTests.fs` with the six facts, plus its `Compile` line after `CatalogueProxyTests.fs`.
- [x] `.contracts-json`: `STORE_BigBoy_0001` is `implemented`, `implementStep` 33.

## Files modified

| File | Change |
|---|---|
| `Berreman/OpticalConstructor/OpticalConstructor.Storage/Errors.fs` | `StorageError.describe` |
| `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueFolderStore.fs` | new |
| `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` | one `Compile` line, no comment |
| `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueFolderTests.fs` | new |
| `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` | one `Compile` line, no comment |
| `specs/0050/.contracts-json` | lifecycle and `implementStep` of this contract only |

## Testing state

**This worker compiled nothing, executed no test, and ran no ELEV analyzer and no invariant command.** The role prompt says
"you act; you run no checks". `gates_run` and `gate_results` are empty. `commit_ready: true` means every requirement of the
slice is addressed; it does not mean any gate passed.

So the six facts have never executed, and neither file has met the compiler. A compile error, a failing fact or an ELEV
finding would first appear at the gate engine.

What was done instead, none of which proves the code compiles or works:

- Each API shape used was matched to a compiled precedent in the repo: curried and static partial application, private
  extension members, `JsonNode` chained `.[ ]` indexing and setting, `JsonValue.Create`, `UTF8Encoding false`, and the
  `Assert` overloads.
- ELEV013, ELEV001, ELEV003, ELEV005, ELEV010, ELEV015 and ELEV019 were read in STL's sources and applied by hand. The gate
  script exempts only `T` findings, so `P` findings count.
- Every fact's outcome was traced by hand through the read and write paths.

Measured, as edit hygiene (AGENTS.md's line-ending rule):

- Every touched source and project file has 0 CR bytes and no BOM.
- `git diff --numstat` equals `--ignore-cr-at-eol` for the tracked `Berreman/` files: 13, 1 and 1 added lines, none removed.
- The six `[<Fact>]` names equal the slice's six test ids exactly.

**Expected count, derived and not measured:** `constructor-unit-tests` should reach 786, the checkpoint of 780 plus 6.

## Artifacts

None. This round captured no logs, because it ran nothing.

## Gotchas

- **The prompts disagree on local checks, and this round followed the role prompt.** "You act; you run no checks"
  (`implement_contract_worker.system-md`) conflicts with "Run the relevant tests after every change" (AGENTS.md). Steps 26
  to 31 ran local builds and tests; step 32 did not. `.manual/` holds no ruling. The cost of this reading is that a defect
  here costs a gate failure rather than a local retry. The base prompt's first section covers that case: the supervisor
  supplies measured diagnostics for the next repair. An operator ruling would settle it for the remaining steps.
- **`.contracts-json` is not in the slice's `edits` list, but this worker edited it.** The role prompt orders it, ASG's
  `executability/receipt.py` says "an IMPLEMENT_CONTRACT worker is ordered to mark its contract implemented", and spec 0038
  step 8 did the same. The working copy holds CRLF while the index holds LF (`attr/text=auto eol=lf`); only the two values
  changed, and the CRLF was kept as found.
- **The slice's "a `SeedSources.tunables` source" is `PresetIds.xenonMonochromator`.** The slice leaves the choice open, and
  slice 0053 names that id when it re-points this file, so the numbers stay the same across the re-point.
- **This file reads code seeds, unlike `CatalogueProxyTests.fs`.** The slice orders it, and slice 0053's edit list includes
  this file, so step 58's deletion can be met. Step 32's route-back does not apply here.
- **Choices the slice leaves open, or where this goes slightly past it:**
  - **Every text is rendered before the first write,** so a serialization failure leaves the folder untouched. The slice
    fixes the write order and the error cases, and both hold.
  - **Extension comparison also uses `OrdinalIgnoreCase`.** The slice states that rule for names; `.MATERIAL-JSON` is the
    same file kind on Windows.
  - **No `[<ElevationBoundary>]` mark was added,** because no signature here carries a primitive.
- **The facts assert a little more than the slice lists, inside the six declared names:**
  - (a) every written file's first byte is `{`, so no BOM;
  - (d) the reason also holds the recorded hash, which exercises `describe`, and a manifest that is not JSON is
    `CatalogueFileRejectedErr` naming `.catalogue-json`, a stated behaviour no listed fact covers;
  - (f) a write blocked on an entry answers `CatalogueIoErr` and leaves no `.tmp` and no manifest, which is the only proof
    of "manifest last" and of the `.tmp` clean-up.
- **What no fact reaches:** the `describe` lines for `SchemaValidationErr`, `SchemaMissingErr`, `BinzExn`,
  `SidecarMissingErr` and `UnsupportedSchemaVersionErr`, and the `CatalogueFileRejectedErr` arm of the write's serialization.
  The slice declares no fact for them.
- **A private free function of one `FolderName` would be ELEV013P.** `isDeclaredHere` means "not FSharp.Core", so STL's types
  count. That is why the read and write bodies are `member private` on `FolderName`.
- **`waveLength` is a label on both `SourcePreset` and `SourcePayloadData`,** so `retunedSource` annotates both the value and
  its return.
- **The tamper fact navigates `payload.Fields[0].waveLength.Fields[0]`,** the shape recorded in
  `.artifacts/0030-json-shapes.log`. A text replace was rejected because the digits could also occur in the recorded hash.
