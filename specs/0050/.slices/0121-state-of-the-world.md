# Where we are

Slice 0121 completes the Details-bay wiring for spec 0050 S3.3 after the Library-window integration in 0120. The selected element now projects the existing catalogue attribution for its bound entry and referenced sample materials below its description and any out-of-band warning. Both authorized facts are implemented; supervisor verification is pending.

# What's working

- Added the public `detailsAttributionLines` model member using the shared catalogue attribution service.
- Appended licence credits and required citations beneath the existing Details title and warning.
- Added the two authorized facts covering attribution, empty/error cases, and real ribbon input followed by visible citation text.

# Tests

No build, tests, analyzers, or invariant gates were run by this worker. The WIRE_UI protocol assigns execution exclusively to the arc-runner. The unchanged roster is build, elevation, all ui-smoke tests, selected ui-tests (`LayerBandsControlsTests`, `OutOfBandBadgeTests`, `TableAndElementRotationTests`), and the eleven s14 invariants. No compilation, warning, test-pass, or commit-readiness claim is made.

Exactly the two specified test identities were added, without the ui-smoke trait. Model assertions cover CC-BY citation, CC0 licence-only attribution, unlicensed built-ins, unbound elements, table/no/stale selection, missing library entries, library and catalogue read errors, and a bound sample's own licence followed by its material's credit. The headless fact opens the real Details tab by its stable id and checks the resulting visible `layerBandsTitle` text. Existing tests and assertions are preserved. The expected count increase is two facts; the supervisor must measure it.

The following repeats checkpoint 119's persisted smoke baseline, not a result from this round. That checkpoint has no `ui_tests` baseline; none is invented or written into the checkpoint file.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

The UI delegates binding resolution to `boundEntry`, reachable attribution identities to `LibraryEntry.attributedEntryIds`, and licence/citation formatting to `CatalogueImportProxy.attributionLinesOf` and `EntryAttributionLine.text`. No attribution rules, persistence shapes, or migrations were introduced. `detailsState` retains its existing band generation and warning derivation and appends the shared text lines in order.

Tests reuse `initMain`'s own import context, the existing sealed-entry fixture factories, the scene update function, the stable ribbon id, and a reactive FuncUI Component under the shared headless session. The later sample version exercises the bound entry's own attribution without replacing the shared catalogue implementation.

# Deferred

No slice implementation requirement is deferred. The supervisor owns the pending gate stage under WIRE_UI invariant 6.

# Gotchas

- `initMain`'s catalogue context and live stores must remain shared so imported fixture materials and samples bind through the same Library proxy used by the view.
- Credit is omitted on either binding-read or attribution-read failure; the existing description and warning projection remains in place.
- The headless Component must notify its state after ribbon input so the semantic-tree assertion observes the actual wired update.
- Source diff numstats agree with and without CR-at-EOL ignoring. The pre-existing supervisor modification to `.manifest.state.json` was left untouched.

# Changelog

- 2026-11-07: Wired Details licence attribution, added exactly the two authorized facts, and handed verification to the supervisor.
