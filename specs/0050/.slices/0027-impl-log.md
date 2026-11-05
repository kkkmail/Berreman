# Step 0027 — IMPLEMENT — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, its gates snapshot, 001 §B,
  .spec-md §4–§6, and the later slices that consume this step's API (30, 31, 123, 124, 129, 146):
  - step 123 moves the SHA-256 body into `ContentHash.ofBytes` and must keep this step's frozen hash;
  - step 124 adds `JsonNode.canonicalFileText` over this step's indented rendering.

  No later slice pins the names of the rendering members, so they are chosen here:
  `indentedCanonicalText` and `compactCanonicalText`.
- No name collision for `nodeOf`, `contentHashOf`, `indentedCanonicalText`, `compactCanonicalText`
  or `canonicalTextWith` in Berreman. STL has a `nodeOf` member in its `DpTests` test project only:
  a member, not a module-level name (ELEV015).
- Production, appended to `OpticalConstructor.Storage/CanonicalJson.fs` after `options`:
  1. `nodeOf<'Value>` is `JsonSerializer.SerializeToNode` through `options`. Every throw is caught and
     answered as `MalformedJsonErr e.Message`, the pattern `ProjectJson` already uses.
  2. `writeCanonical` is a private recursive `Utf8JsonWriter` writer:
     - objects are sorted by `StringComparer.Ordinal`;
     - arrays keep their order;
     - `null` is written as `null`;
     - a value writes itself through `JsonNode.WriteTo`, which keeps the serializer's raw number text.
  3. Two private `JsonWriterOptions`, both `JavaScriptEncoder.UnsafeRelaxedJsonEscaping`:
     - indented: two spaces, `' '`, `NewLine = "\n"`;
     - compact.
  4. `type JsonNode with`:
     - a private `canonicalTextWith (writerOptions)`, which renders through an `ArrayBufferWriter<byte>`
       and decodes UTF-8 (`Utf8JsonWriter` writes no BOM);
     - `indentedCanonicalText`;
     - `compactCanonicalText`.
  5. `contentHashOf<'Value>` is `Convert.ToHexStringLower(SHA256.HashData(UTF8 compact text))`, wrapped
     in `ContentHash`.
- First build: the shell turned the `"\n"` escapes into real newlines inside two lines. I fixed both
  with exact edits. The Storage project then built with 0 warnings and 0 errors.
- Tests: five facts appended to the existing `CanonicalJsonTests` module, under the exact slice ids.
  The three facts from steps 22/23 are unchanged. Three helpers are test-local:
  - `okOf` over `StorageError`;
  - `tunableSourceAt`, a tunable `SourcePreset` with a GUID-text id;
  - `keysReversed`, which rebuilds every `JsonObject` with its properties in reverse order, keeps
    array order and `DeepClone`s values.

  `objectKeysOf` collects every object's key list from the rendered text.
- Frozen literals for (d):
  - I wrote the indented text literal by hand, from the writer's rules.
  - I predicted the hash independently with Python, `hashlib.sha256` over the UTF-8 compact text:
    `74b4c2774e9b111cf97a9d595611a1557aca2e09cec11ffc7fadfd19e7163e90`.
  - I captured the writer's output with a placeholder hash. The text assertion passed as written, and
    the actual hash equalled the Python prediction (`.artifacts/0027-canonical-json-first-run.log`).
  - Both literals are now frozen in the test.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs` (+61 / −0): eight
  `open`s, `nodeOf`, `writeCanonical`, the two writer options, the `JsonNode` extension
  (`canonicalTextWith`, `indentedCanonicalText`, `compactCanonicalText`) and `contentHashOf`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs` (+138 / −2): seven
  `open`s, the module doc widened to the canonical form, the helpers above and facts (a)–(e). No
  `.fsproj` change: the file was already in the compile list.

## Testing state

The arc-runner's gate engine is the gate authority. These are local observations only, made so the
round ends on code that compiles and passes:

- `build` command (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0,
  `0 Warning(s)`, `0 Error(s)` (`.artifacts/0027-build.log`).
- The constructor suite, whole (`dotnet test --no-build -c Release -p:Platform=x64 ...`):
  `Test run summary: Passed!`, total 759, succeeded 759, failed 0, skipped 0. That is 754 from step 26
  plus the five new facts (`.artifacts/0027-constructor-unit-tests-whole.log`).
- `--list-tests --filter-class OpticalConstructor.Tests.CanonicalJsonTests` discovers 8 facts: the
  three from steps 22/23 and the five slice ids exactly as written.
- The ELEV script (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`) reports
  `ELEV-GATE: OK - 0 new finding(s) in 5 project(s), 127s` with 0 known findings. Storage was expanded
  to its dependents: App, TestWindows.App, TestWindows and Ui. The test file is out of the analyzers'
  scope (`.artifacts/0027-elevation-local.log`).

Every requirement of the slice is delivered in this round:
- (1) `nodeOf`, with `MalformedJsonErr`;
- (2) the two canonical renderings as `JsonNode` extension members;
- (3) `contentHashOf`;
- tests (a) to (e).
- The six s04 invariants, decoded from the gate snapshot and run with `git grep`: each finds nothing
  (exit 1).
- Line endings: both files are LF with zero CR bytes, and `git diff --numstat` equals
  `--ignore-cr-at-eol` (61/0 and 138/2).

## Artifacts

- `.artifacts/0027-build.log`
- `.artifacts/0027-canonical-json-first-run.log` (the capture run with the placeholder hash)
- `.artifacts/0027-constructor-unit-tests-whole.log`
- `.artifacts/0027-elevation-local.log`
- `.artifacts/0027-null-node-check.log` (the scratch script and its output)

## Gotchas

- `SerializeToNode` serializes and then parses into a node. A number therefore stays a
  `JsonElement`, and `WriteTo` copies its raw text: the shortest round-trip text, invariant culture,
  that the serializer wrote. A string is unescaped and re-escaped by the canonical writer's own
  encoder, so the relaxed escaping applies even though `options` escapes non-ASCII.
- `member private` on an optional type extension compiles, and keeps the one rendering body off the
  public surface.
- A `\n` inside a Python heredoc run through the Bash tool reached the file as a real newline. Edit
  string literals with escapes through the Edit tool.
- `specs/0050/.manifest.state.json` was already modified when the round started. It is the
  supervisor's file, and this round leaves it alone.
- `nodeOf` of a value that serializes to JSON `null` (e.g. `None`) answers `Ok null`. The extension
  members compile to static calls, so a null node renders as `null` and hashes as SHA-256 of `null`.
  This was checked once with a scratch `dotnet fsi` script over the Release output
  (`.artifacts/0027-null-node-check.log`); the scratch folder was deleted afterwards.
