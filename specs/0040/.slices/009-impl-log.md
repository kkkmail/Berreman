# Step 009 — impl-log

Slice: `009.slice-md` — IMPLEMENT. Give `Sample` a geometry-constrained
`supportedEmission : Emission` (spec 0040 Part D.2 / operator 010/Q3), reusing the
`Emission` DU; a `ThinFilm` is pinned to `EmitReflectedOnly` (unclearable), a
`Plate` defaults to `EmitBoth`.

## Progress

- [x] Read system prompt, project prompt, slice spec, surrounding code.
- [x] Add `supportedEmission` field + `defaultSupportedEmission` /
      `constrainEmission` / `withSupportedEmission` helpers (`ElementId.fs`).
- [x] Set the field on all 11 seeds.
- [x] Update `SampleEditorView.toSample` and every test `Sample` literal.
- [x] Add unit tests for the invariant.
- [x] Write state-of-the-world.

## Files modified

- `OpticalConstructor.Domain/ElementId.fs` — new field + 3 helpers + 11 seeds.
- `OpticalConstructor.Ui/SampleEditorView.fs` — `toSample` sets the field.
- `OpticalConstructor.Tests/SampleProxyTests.fs` — 4 new invariant facts.
- `OpticalConstructor.Tests/LibraryFacetsTests.fs` — 2 literals updated.
- `OpticalConstructor.Tests/LibraryProxyTests.fs` — 2 literals updated.
- `OpticalConstructor.Tests/PropagationTests.fs` — 3 literals updated.
- `OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs` — 1 literal updated.
- `OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs` — 3 literals updated.

## Testing state

Per IMPLEMENT worker Invariant 6, this session runs NO gates; the arc-runner's
deterministic gate engine runs `build` / `unit-tests` / `constructor-unit-tests` /
`ui-smoke` / `ui-tests` after exit. Local reasoning: the only production semantic
change is a new record field plus three pure helpers; all other edits are
mechanical field additions so every `Sample` literal compiles. `commit_ready:
true`.

## Artifacts

None (pure Domain + test change; no captured logs/screenshots).

## Gotchas

- **Touched `OpticalConstructor.Ui` + `.Ui.Tests` beyond the declared
  `touches: [Domain, Tests]`.** Adding a REQUIRED record field breaks every
  `Sample` literal across the whole `.slnx`; the `build` gate compiles the whole
  solution and `ui-smoke`/`ui-tests` run the UI test projects. The slice mandates
  "Update every `Sample` constructor and consumer mechanically to compile", so
  these are compile-only field additions (`SampleEditorView.toSample` + two Ui.Tests
  literals), nothing more.
- **`SampleStore.payloadOf` was NOT changed.** The versioning rule's payload is
  `physics = s.structure`, `metadata = { name; description; substrate }` — it never
  reads `supportedEmission`. Since the field is a pure function of `substrate`
  (already in metadata), leaving the payload untouched keeps `decideVersioning`
  correct and is the minimal change. Verified no versioning test regresses.
- **`Sample` is NOT serialized.** Storage only references the `CatalogueKind.Sample`
  enum value in its JSON schemas; the sample library is an in-memory proxy. No
  schema / round-trip change needed.
- **Editor `toSample` resets emission to the geometry default on save.** There is no
  emission control in the sample editor yet (out of scope, Domain-only slice), so
  `toSample` derives `supportedEmission = defaultSupportedEmission m.substrate`.
  Deferred: wiring an emission control that preserves an edited value.
- **Tests live in `SampleProxyTests.fs`** — the same file the step-008
  `validateSample` Sample-domain facts landed in; `Emission` cases are referenced
  qualified as `Placement.…` (no new `open`).
