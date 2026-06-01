# Architecture critique -- 015.slice-md cycle 1

## Summary

Clean, with two small notes worth the judge's attention. The slice lands exactly what J.6–J.9 asked for: `EnvironmentSettings`/`Preferences` persisting as schema-validated JSON over the *reused* slice-003 `ProjectJson.options`, an `AppShell` authored against the public MIT FuncUI DSL with the audit-gated clone left unreferenced and unresolved, and pure `Result`-returning `Validation` boundary validators. The single most important finding is a latent inconsistency in the env `validate` error-message extraction (it diverges from `SchemaValidation.collectMessages` and likely collapses to a generic message), which is currently harmless only because `load` swallows the error.

## Layering

No violations. The new Ui modules depend downward only: `UserEnvironment` opens `OpticalConstructor.Storage` (for `ProjectJson.options`/`StorageError`), `OpticalConstructor.Domain.Units`, and the engine (`Berreman.Solvers`, `Berreman.Media`); `AppShell` depends on `UserEnvironment` plus Avalonia/FuncUI. Direction is Ui → Storage → Domain → engine, never the reverse. The FuncUI-clone gate is respected: `OpticalConstructor.Ui.fsproj:77` references the public `Avalonia.FuncUI` 1.6.0 NuGet, the comments explicitly disclaim the clone, and no `Avalonia.FuncUI.Clone` path or project reference appears — AC-J8's linking-mechanism-unresolved requirement holds.

## Separation of concerns

The persisted layout types (`Theme`, `DockSide`, `PanelState`, `PanelLayout`) correctly live in `UserEnvironment` (they are `EnvironmentSettings` fields and must serialize), while `AppShell` owns the Avalonia mapping and the FuncUI view — a sensible split. One mild seam question: the *pure, Avalonia-free* layout reducers `setPanelVisible`/`dockPanel`/`visiblePanels` (`AppShell.fs:60-76`) sit in the Avalonia-dependent module, so `EnvironmentRoundTripTests` must reach through `OpticalConstructor.Ui.AppShell` to edit a layout (`EnvironmentRoundTripTests.fs:58`). Co-locating those reducers with the `PanelLayout` type in `UserEnvironment` would keep pure layout edits behind the thin, Avalonia-free seam and out of the headless test's dependency surface. Defensible as-is (the shell is the conceptual home of panel editing); noted for the next slice.

## Consistency

Strong overall. The slice reuses the established patterns: lazy single-load schema behind a distinct `$id`, `StorageError` for IO/validation failures, the shared `ProjectJson.options` (so fieldless DUs → bare strings — matching the schema's `theme`/`dockSide`/`unitOfMeasure` enums — and multi-field unions → the `{Case, Fields}` adjacent-tag shape the `favoritePin` `$def` encodes). `Severity = Blocking | Warning` to dodge the `Result.Error` clash is a tidy, well-documented call.

One divergence: `UserEnvironment.validate` (`UserEnvironment.fs:228-241`) reimplements the storage validator's message extraction but reads only the top-level `results.Errors`, whereas `SchemaValidation.collectMessages` (`SchemaValidation.fs:42-56`) walks `results.Details` recursively. Under `OutputFormat.List` the per-keyword errors live in the flattened `Details` nodes, not the root `Errors`, so the env path will almost always fall through to the generic `"environment schema validation failed"` string rather than the library's specific messages. Validity itself (`results.IsValid`) is still computed correctly, so the defaults-fallback is unaffected — but the message fidelity silently differs from the project schema's. Since `collectMessages` is `private`, the cleanest fix is to lift a shared `collectMessages`/`validate(schema, element)` helper that both call.

## Spec fit

Requirements R-1…R-4 are all delivered and AC-J6/J7/J8/J9 are exercised by the two new test files (20 facts). Two scope notes:

- **Shared job-status area (R-3 / files-in-scope line 41).** The slice's files-in-scope names `AppShell.fs … dockable panels, theme, shared job-status area`, but `shellView` renders only the five layout panels and `AppShell.fs` contains no job/status/runner region or placeholder panel (grep confirms zero matches). The non-requirements correctly defer the *runner* to slice 016, but the *host area* it plugs into was named here. This is a minor under-delivery: slice 016 now has to introduce the status seam as well as the runner. A placeholder panel (e.g. a `"status"` `PanelState`) or a documented hand-off line would have de-cornered 016. Defensible deferral, but flag it so the judge sees the seam isn't pre-cut.
- **Physical-sanity warnings (R-4).** Only `imaginaryIndexGainWarning` is delivered; non-causal/Kramers–Kronig detection is deferred. The spec's "(e.g. …)" framing plus §0 constraint 6 (minimum implementation) make one representative check a reasonable read, and the worker recorded the choice in the SoW `Deferred`. Acceptable.

## Evolvability

The env persistence duplicates the slice-003 load/validate machinery (`envSchema` lazy load, `validate`, `serialize`/`deserialize` envelope handling) rather than parameterizing the storage helpers by schema path. The worker justified this — the env schema lives in the Ui project per files-in-scope and `JsonSchema.Net` flows transitively — and the duplication is small, but it means a future change to the §A.7 validate-on-load convention must be made in two places. If a third schema joins the family, lifting a `validateAgainst (schema) (element)` helper into Storage becomes worthwhile.

## Risks

- **Generic env validation messages (latent).** Per the Consistency note, env-schema failures currently surface only the fallback string. Harmless today because `load` discards the error and returns `defaults`; it bites the moment a future slice surfaces env-validation errors to the user. Low severity, easy fix.
- **Unguarded palette parse.** `paletteColors` calls `Color.Parse` on every persisted `chartPalette` string (`AppShell.fs:51`), and the schema constrains those items only to `type: string` (no hex `pattern`). A malformed persisted color would throw at render time rather than degrade gracefully. The view binding is deferred, so this is dormant, but a `pattern` on the schema item or a `tryParse`-and-skip in `paletteColors` would keep the gain in the headless layer.

## Bottom line

This is shippable. Layering, the FuncUI-clone gate, the reuse-not-reinvent discipline, and the validation purity are all satisfied, and the test suite proves the round-trip, the defaults-fallback, and the validators. The only substantive item is the env `validate` message-extraction divergence — currently invisible but worth a one-line fix or a shared helper before any slice surfaces env errors. The missing job-status host area and the KK-warning deferral are notes for slice 016, not blockers. I'd lean ship; the judge owns the verdict.
