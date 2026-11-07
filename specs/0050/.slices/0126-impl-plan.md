# Implementation plan

Implement `STORE_BigBoy_0004` against the existing three-field `KitProxy` and the immutable `S41-KIT-FOLDER` obligation. Move the catalogue's atomic write body to `FileName.writeBytesAtomically`, retain the text writer as its UTF-8 caller, and implement folder-backed kit initialisation, reading and replacement in `KitFolderStore.fs`. Construction must have no filesystem effects; metadata is written last and all filesystem failures become typed kit errors.

Add `KitProxyFolderTests` in the existing `KitProxyTests.fs`, inheriting all five contract facts unchanged and adding exactly the two authorized filesystem facts. Use GUID-named folders under the test output and the existing kit fixtures. Cover missing folders, UTF-8/BOM reading, unsupported schema versions and malformed JSON without additional test identities.

Update the Domain proxy documentation and the current arc's contract lifecycle while preserving declaration metadata. Write the required implementation log and state-of-the-world at the supplied paths. Preserve the supervisor's pre-existing `.manifest.state.json` change and all gate, census, checkpoint and version metadata.

Risks are accidental filesystem effects during construction, loss of pre-existing files, leaking IO exceptions and duplicating atomic-write logic. Keep the existing catalogue contract facts unchanged. Per the IMPLEMENT_CONTRACT act-only protocol, the supervisor alone runs build, elevation, constructor-unit-tests and the eleven s15 invariants; this worker reports no gate results.

## Attempt 02 repair

The supervisor reports one ELEV010 finding on the atomic writer's `byte[]` parameter. Reuse `Softellect.Sys.Primitives.FileByteContent`, already consumed by the repository's canonical JSON hashing, as the parameter type. Bind UTF-8 output directly as `FileByteContent` in both callers and unwrap it only at `File.WriteAllBytes`. This changes only `CatalogueFolderStore.fs` and `KitFolderStore.fs`; the atomic algorithm, tests and acceptance remain intact.

The slice's literal `byte[]` signature conflicts with the measured ELEV010 rule. AGENTS.md explicitly gives the analyzers precedence, and the retry authorizes repair within the declared effects. Record this necessary signature deviation without modifying the frozen spec or adding a compatibility overload. Preserve the existing implemented contract lifecycle and leave verification to the supervisor.
