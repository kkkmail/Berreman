# Step 0027 — IMPLEMENT — state of the world

## Where we are

This is spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content
hash, schemas".
- Steps 24–26 gave Domain the entry vocabulary, its content and envelope, and the catalogue manifest.
- Steps 21–23 moved the one serializer options value into `CanonicalJson` and taught it complex
  numbers and real matrices.

This step adds the canonical form and the content hash in `OpticalConstructor.Storage/CanonicalJson.fs`
(001 §B: `contentHash` is SHA-256 over canonical JSON, with sorted keys and invariant-culture
numbers; .spec-md §5). Later steps build on it:
- step 30 (`sealed`, `fileText`, `parseEntry`) and step 31 (the manifest's `fileText` and
  `manifestHash`) consume `nodeOf`, `indentedCanonicalText` and `contentHashOf`;
- step 123 moves the SHA-256 body into `ContentHash.ofBytes`;
- step 124 adds `JsonNode.canonicalFileText`.

## What's working

- Added `CanonicalJson.nodeOf`: a value's `JsonNode` through the shared options. Any serializer
  failure, a non-finite double included, is `MalformedJsonErr`, never a throw.
- Added the canonical renderings as `JsonNode` extension members:
  - `indentedCanonicalText` (the file form: two-space indents, `\n` line ends);
  - `compactCanonicalText` (the hashed form).

  Both sort every object's keys ordinally, keep array order and the serializer's invariant
  shortest round-trip numbers, escape relaxed (`CO₂ µm` stays readable) and write no BOM.
- Added `CanonicalJson.contentHashOf`: the lowercase hex SHA-256 of the compact canonical text's
  UTF-8.
- Added five `CanonicalJsonTests` facts: key order, bit-exact doubles under `de-DE`, the hash
  definition, a frozen text and hash, and the non-finite refusal. The constructor suite is 759/759.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0027-build.log`).
- `constructor-unit-tests`, whole suite: 759 total, 759 succeeded, 0 failed, 0 skipped. That is 754
  plus the five new facts, each discovered under its slice id
  (`.artifacts/0027-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)` (Storage plus its dependents App,
  TestWindows.App, TestWindows and Ui), with 0 known findings (`.artifacts/0027-elevation-local.log`).
- The six s04 invariants: each grep finds nothing.

```yaml
gates:
  constructor_unit_tests: 759
```

## Architecture

- **One body renders both forms.** The private recursive `writeCanonical` drives a `Utf8JsonWriter`.
  The two forms differ only in their `JsonWriterOptions`, which a private `canonicalTextWith` member
  takes. Bare `string` therefore appears only on `JsonNode` members, which may speak primitives. No
  free function takes or returns a `string` or `byte[]` (ELEV003 / ELEV003P).
- **Numbers are never reformatted.** `SerializeToNode` parses the serializer's own output, so a number
  node is a `JsonElement`, and `WriteTo` copies its raw text. That text is the invariant shortest
  round-trip form System.Text.Json writes, which reads back with correct IEEE rounding. The rendering
  is independent of `CultureInfo.CurrentCulture` by construction.
- **Escaping is the canonical writer's own.** Strings in the node are unescaped and re-escaped by the
  writer's `JavaScriptEncoder.UnsafeRelaxedJsonEscaping`, whatever encoder `options` used to build
  the node.
- **The hash is over the compact form only**, so it does not depend on whitespace, key order or
  culture. The file form (step 30) is the indented text, and its hash is recomputed from the parsed
  content.

## Deferred

Nothing. Every requirement of the slice landed this round:
- (1) `nodeOf` with `MalformedJsonErr`;
- (2) both canonical renderings as `JsonNode` extension members, with ordinal keys, kept array order,
  serializer number text, relaxed escaping, no BOM, two-space and `\n` indents, and a compact form;
- (3) `contentHashOf`;
- tests (a) to (e).

The final `\n` of a file is step 124's `canonicalFileText`, by the spec's own plan.

## Gotchas

- **Frozen literals.** The frozen text and hash in `the canonical form of a fixed value is frozen` are
  the writer's output, captured in this step. The hash
  `74b4c2774e9b111cf97a9d595611a1557aca2e09cec11ffc7fadfd19e7163e90` also matches an independent
  Python `hashlib.sha256` over the expected compact text.
  - Never re-capture them to make a later change pass: a change there changes every committed
    catalogue file's hash (S1.5).
  - Step 123 must keep them green when it moves the hashing into `ContentHash.ofBytes`.
- **Rendering a null node.** `nodeOf` of a value that serializes to JSON `null` (e.g. `None`) answers
  `Ok null`. The extension members compile to static calls, so a null node renders as `null` rather
  than throwing.
- **Opening the module.** The rendering members are optional extensions: a caller needs
  `open OpticalConstructor.Storage.CanonicalJson` to see them.
- **Shell escapes.** A `\n` inside a Python heredoc run through the Bash tool reached the file as a
  real newline. Edit escaped string literals with the Edit tool.

## Changelog

- 2026-11-05: step 0027 added `CanonicalJson.nodeOf`, the `JsonNode` canonical renderings
  (`indentedCanonicalText`, `compactCanonicalText`) and `CanonicalJson.contentHashOf`, plus five
  `CanonicalJsonTests` facts with a frozen text and hash (759/759).
