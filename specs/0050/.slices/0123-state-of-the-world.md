# Where we are

Slice 0123 implements spec 0050 S4.1's path, file-kind and shared byte-hash contract under immutable obligation `S41-KIT-PATHS`. Attempt 02 repairs the supervisor's measured ELEV010 on the hash signature by reusing `FileByteContent`. The kit shapes and authorized test effects remain intact. Supervisor verification of the repair is pending.

# What's working

- Added kit-relative paths with forward-slash conversion and rejection of blank, rooted, colon-bearing and invalid-segment paths.
- Added the five kit file kinds, shared file text and lab-file data, plus missing-file and invalid-path errors.
- Moved byte hashing into `ContentHash.ofBytes` and reused it for canonical JSON content hashes.
- Added the three authorized facts and extended the existing kit-error descriptions without removing existing assertions.
- Elevated the hash input with the existing `FileByteContent` and updated canonical JSON and the hash fixtures at origin.

# Tests

No build, test, analyzer or invariant gates were executed by this worker. The IMPLEMENT protocol assigns them exclusively to the arc-runner after this session. The resolved roster is build, elevation, all discovered constructor-unit-tests and the eleven s15 invariants. The retry task reports that the prior review and build passed and elevation failed on the raw byte-array hash signature. Those observations predate this repair; no new pass, measured increase or commit-readiness claim is made.

The new path fact covers the required valid and invalid examples, mixed separators, Windows-rooted and UNC forms, trailing separators, standalone dot segments, colon-bearing streams and unchanged valid text. The kind fact covers the ordered five kinds, their exact extensions and filenames, including the required experiment name. The hash fact asserts the supplied lowercase SHA-256 literals for UTF-8 `abc` and empty bytes. The existing kit-error fact now asserts all seven distinct descriptions. The frozen canonical JSON hash test remains unchanged.

Exactly three fact identities were added. The following is the existing baseline recorded in checkpoint 121, not a result measured by this worker; the supervisor owns the next count and checkpoint.

```yaml
gates:
  constructor_unit_tests: 1018
```

# Architecture

`KitRelativePath.tryCreate` performs only separator replacement and the specified lexical rejection. It preserves all other accepted text. `KitFileKind` names repeated files through `FileStem.withExtension`; the singleton kit file stays outside that union. The kind is represented by its extension, with no additional wire tag or decoder.

`KitFileText` serves both app-owned and read-only lab files. `KitDataFile` carries an elevated path, text and stored-byte hash. The two new error cases retain diagnostic payloads and render through `KitError.describe`.

`ContentHash.ofBytes` owns the SHA-256 and lowercase hexadecimal implementation moved from Storage. It accepts the existing `Softellect.Sys.Primitives.FileByteContent` and unwraps only at the hashing interop call. `CanonicalJson.contentHashOf` supplies the same compact canonical UTF-8 bytes as before, elevated at origin; the authorized hash fact does the same for its two fixtures. No stored format, schema version or existing hash rule changes; no migration or compatibility path is introduced.

The elevated input deliberately replaces the slice how-to's `byte[]` annotation because measured ELEV010 forbids that mixed signature and AGENTS.md gives analyzers precedence. This stays within the declared files and the `ContentHash.ofBytes` creation effect, reuses a lower-level type and preserves every acceptance obligation. It introduces no new contract type or test identity.

# Deferred

No slice implementation requirement is deferred. Gate execution is the supervisor's pending stage under IMPLEMENT invariant 6.

# Gotchas

- Blank input is rejected before separator replacement, which also prevents a null input from throwing at the external-text boundary.
- Later folder operations must use the specified path contract; this slice adds no filesystem operations or proxy verbs.
- The five edited source files use LF, and their diff numstats agree with and without ignoring CR at line ends.
- The pre-existing supervisor changes to `.manifest.state.json` and `.expected-red-observed-json` remain untouched. No census, gate snapshot, baseline, schema or project file was edited.
- Subsequent kit-file readers must elevate bytes as `FileByteContent` where they are read before calling `ContentHash.ofBytes`; a primitive overload would restore the measured ELEV010 failure.

# Changelog

- 2026-11-07: Added kit path and file-kind contracts, consolidated byte hashing, and authored the authorized test effects for supervisor verification.
- 2026-11-07 (attempt 02): Repaired the mixed primitive/domain hash signature using `FileByteContent`, preserving hash outputs and test identities; recorded the analyzer-driven deviation for supervisor verification.
