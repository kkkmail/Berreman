# Reuse critique -- 013.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on
  `Berreman/OpticalConstructor/OpticalConstructor.Storage` and
  `.../OpticalConstructor.Tests`, plus the slice-003 storage core —
  `ProjectJson.fs`, `SchemaValidation.fs`, `ProjectFile.fs`, `Errors.fs` —
  and the slice-004 `MaterialImport.fs` / `MaterialLibrary` domain).
- Files inspected: ~16/200 (4 new `.fs`, the 2 new test files, the 2 edited
  `.fsproj`, the `.gitignore` edit, plus the existing helpers the diff layers
  on and the test-project fixture pattern).
- Extensions: `.fs`, `.fsproj`, `.json`.
- Cycle-1 status: F1 (the hand-rolled `categoryName`/`categoryOfName`
  `MaterialCategory`↔string map) is **resolved** — the DTO field is now
  `category : MaterialCategory` (`Report.fs:39`) and export boxes `e.category`
  straight into the shared `ProjectJson.options` (`Report.fs:139`); both
  helpers are gone. Cycle-1 F2/F3 were judge-deferred and are re-checked below.

## Findings

### F1: `materialEntrySchema` re-spells the schema path literal and re-implements `SchemaValidation`'s load/evaluate ladder (cycle-1 F2, still open)

- **Worker added:** `MaterialLibrary.materialEntrySchema` (`Report.fs:60-75`),
  which re-derives the schema location as
  `Path.Combine(AppContext.BaseDirectory, "schema", "optical-constructor-project.schema.json")`
  (`Report.fs:62-63`), re-checks `File.Exists`, and re-maps the failure to
  `SchemaMissing`/`FileIoError`; plus `validateEntryElement` (`Report.fs:79-86`),
  which re-creates `EvaluationOptions(OutputFormat = OutputFormat.List)`, calls
  `schema.Evaluate`, and tests `results.IsValid`.
- **Existing helper:** `SchemaValidation.fs` — the **public**
  `schemaRelativePath` (`SchemaValidation.fs:17-18`) already names exactly
  `Path.Combine("schema", "optical-constructor-project.schema.json")`; the
  `private schema` lazy (`:29-36`) already does the `File.Exists` →
  `SchemaMissing` / read → `FileIoError` ladder; and `validate` (`:61-73`) +
  `collectMessages` (`:42-56`) already encapsulate the
  `EvaluationOptions(... List)` → `Evaluate` → `IsValid` → message-collection
  shape.
- **Why it matters:** the schema-file path is now spelled out as a literal in
  two modules. If the build-copied content item is ever renamed or relocated,
  `SchemaValidation` updates via its one constant but `Report.fs` keeps loading
  the stale path and the material-export validation silently regresses to
  `SchemaMissing`. The evaluate logic is also degraded on copy: where
  `validate` surfaces JsonSchema.Net's per-keyword messages via
  `collectMessages`, `validateEntryElement` discards them for a single
  hard-coded `"document violated the materialEntry $def"` string
  (`Report.fs:86`), so an `exportMaterials` validation failure is far harder to
  diagnose than an `openProject` one. `schemaRelativePath` was made public —
  the only public surface in `SchemaValidation` — which reads as an explicit
  invitation to reuse it; the diff declines it.
- **Suggested action:** at minimum reuse
  `SchemaValidation.schemaRelativePath` (combined with `AppContext.BaseDirectory`)
  for the path constant. Building a *distinct* `$ref`-rewritten single-entry
  schema document is legitimate (you cannot reuse `validate`, which validates a
  whole project envelope), but the path constant and the message-collection
  should come from the one module that owns them — e.g. expose
  `collectMessages` (or a small `evaluate`) from `SchemaValidation` and call it
  here so both validators report failures with identical fidelity.

### F2: the per-user app-data root is re-spelled as a literal across two new modules

- **Worker added:** the same four-segment app-data root
  `Path.Combine(GetFolderPath ApplicationData, "Softellect", "Berreman", "OpticalConstructor", …)`
  in `RecentFiles.prefsPath` (`RecentFiles.fs:28-33`, segments at `:30-32`)
  and again in `Report.reportsDir` (`Report.fs:270-273`, segments at `:271-273`).
- **Existing helper:** `RecentFiles.prefsPath` establishes the convention
  first (it compiles before `Report.fs` in the fsproj `<Compile>` order), and
  `Report.reportsDir` re-derives the identical three product segments before
  appending `"reports"`. There is no pre-existing app-data helper in the tree
  (grep finds these two call sites only), so strictly this is intra-slice
  duplication between two files the same slice introduces, not divergence from
  an established helper.
- **Why it matters:** the product's app-data location is now defined twice; a
  re-brand of the `"Softellect"/"Berreman"/"OpticalConstructor"` root (or a move
  to `LocalApplicationData`) must be edited in two places or the recent-files
  prefs and the reports folder silently diverge to different roots. The cost is
  small and both call sites are new, so this is a low-severity near-miss, not an
  architecture breach.
- **Suggested action:** optional — hoist the shared
  `…/Softellect/Berreman/OpticalConstructor` root into one private helper (it
  could live in `RecentFiles`, which already owns the first use) and have
  `reportsDir` append `"reports"` to it. Defensible to leave as-is for a
  §I.5–I.8 slice given both sites are net-new and trivially short; flagged so
  the judge can weigh it.

### F3: the two new test files re-copy the `vacuumSystem`/`node`/`sampleProject` fixture verbatim (cycle-1 F3, unchanged)

- **Worker added:** byte-for-byte identical `filmThickness`, `vacuumSystem`,
  `light`, `node`, and `projectWith`/`sampleProject` fixture blocks in
  `HistoryTests.fs:22-40` and `ExportImportTests.fs:78-95`.
- **Existing helper:** `ProjectJsonRoundtripTests.fs` already defines the same
  `vacuumSystem` + `node` + `sampleProject` trio, and `RoundTripTests.fs` /
  `BeamTreeTests.fs` carry near-copies — the test project's established
  convention is one private fixture block per file.
- **Why it matters:** real duplication (a change to `OpticalSystem`'s shape now
  touches five-plus files), but it is *consistent* duplication that matches the
  in-repo convention rather than diverging from it; the cost is pre-existing,
  not introduced by this slice's design.
- **Suggested action:** leave as-is for this slice — there is no shared fixture
  module to reuse today, and the judge already deferred this in cycle 1. If the
  duplication is to be arrested it should be a separate, spec-traced refactor
  that extracts one shared `OpticalConstructor.Tests` fixture helper for all of
  these files at once, out of scope for a persistence-II slice.

## Bottom line

Cycle 1's load-bearing reuse finding (F1, the second hand-rolled DU
serialization) is genuinely fixed — the material DTO now rides the one shared
JSON stack on both sides. What remains is the same secondary `SchemaValidation`
reuse gap the cycle-1 critic raised and the judge consciously deferred (now F1
here): a re-spelled schema-path literal and a message-discarding re-implementation
of an existing, now-public helper — small, local, and still worth a cleanup but
not behaviour-affecting. The new F2 (duplicated app-data root) and F3
(pattern-conforming test fixtures) are minor and largely intra-slice. My read:
the cycle-2 diff discharged the one finding that moved the cycle-1 verdict;
nothing here rises to architecture-breaking, and the judge can reasonably route
F1/F2 as in-place suggestions (or accept them as deferred) rather than a
re-spawn.
