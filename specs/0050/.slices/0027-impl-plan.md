# Step 0027 — IMPLEMENT — impl plan

## Approach

The canonical form and the content hash (001 §B: `contentHash` is SHA-256 over canonical JSON,
sorted keys, invariant-culture numbers; .spec-md §5), all in
`OpticalConstructor.Storage/CanonicalJson.fs`, after the shared `options`:

1. `nodeOf<'Value> (value : 'Value) : Result<JsonNode, StorageError>` is
   `JsonSerializer.SerializeToNode` through `options`; any serializer throw (a non-finite double, an
   unsupported type) is caught and answered as `MalformedJsonErr e.Message`, as `ProjectJson` does.
2. A private recursive writer over `Utf8JsonWriter`: a `JsonObject` writes its properties sorted by
   `StringComparer.Ordinal`; a `JsonArray` keeps its order; a `null` is `null`; a `JsonValue` writes
   itself (`WriteTo`), so a number keeps the serializer's invariant shortest round-trip text. Two
   private `JsonWriterOptions`, both with `JavaScriptEncoder.UnsafeRelaxedJsonEscaping`: indented
   (two spaces, `IndentCharacter ' '`, `NewLine "\n"`) and compact. `Utf8JsonWriter` writes no BOM.
3. `type JsonNode with` — `indentedCanonicalText : string` (the file form) and
   `compactCanonicalText : string` (the hashed form), both over one private member that renders with
   given writer options. Members may speak primitives; no free function takes or returns a bare
   `string` / `byte[]` (ELEV003 / ELEV003P).
4. `contentHashOf<'Value> (value : 'Value) : Result<ContentHash, StorageError>` is
   `Convert.ToHexStringLower (SHA256.HashData (UTF-8 of nodeOf value's compact canonical text))`.

Tests, appended to the existing `CanonicalJsonTests` module (steps 22/23 created it), under the
five exact slice ids: (a) key order over a test-local tunable `SourcePreset` and a recursively
key-reversed copy; (b) eight doubles bit-exact through both renderings under `de-DE`, restored in a
`finally`, `.` decimal point, text equal to the invariant-culture text; (c) hash equals the test's own
`SHA256.HashData` over the compact text's UTF-8, 64 lowercase hex, equal values equal, changed number
different; (d) a test-local `MaterialCategory` (fixed GUID, name `Glass — CO₂ µm`) whose indented
text and hash equal literals captured once from the implemented writer; (e) `nodeOf nan` is
`MalformedJsonErr`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`

## Risks

- `member private` on an optional extension: if F# refuses it, fall back to a private
  `Utf8JsonWriter`-level helper with no primitive in its signature.
- ELEV003P / ELEV010P on private helpers: keep every private function's signature free of bare
  primitives (writer, node, options only); the string lives only on the members.
- Record-label shadowing in the test file: annotate the test-local `SourcePreset`, `MaterialCategory`
  and `WaveLengthInterval` values; qualify `LibraryEntryFiles.ContentHash` instead of opening the module.
- The frozen literals in (d) must be the writer's actual output: captured by running the writer once
  after the build, then pasted.
