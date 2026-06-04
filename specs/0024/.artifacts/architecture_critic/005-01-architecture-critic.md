# Architecture critique -- 005.slice-md cycle 1

## Summary

Clean slice. `SourceView.fs` renders the Part U4 sources panel and wires it into the
composition root exactly along the slice-002/003/004 view precedent: a sub-state +
dispatch view, a thin view-local message union delegating to frozen reducers, and the
two plots routed through the slice-002 `ChartHosts` adapters. Every R-1/R-2 obligation
and both ACs are met, no frozen module is touched, and the SoW reconciles with the diff.
The only items worth a note are low-severity test-brittleness and a foreseeable
root-model reshape, neither blocking.

## Separation of concerns

`SourceView` re-implements no reducer or projection. `update` delegates `Edit` to the
frozen `SourceEditorView.update` and `ApplyPreset` to `PolarizationPicker.applyPreset`
(`SourceView.fs:60-63`); the readout reads `liveStokes`/`poincareMarker`/
`ellipseParameters` straight from the picker (`SourceView.fs:194-208`); the plots are
built by `PolarizationPlots.renderEllipse`/`poincareSphere` and handed to
`ChartHosts.scottPlotHost`/`webView2Host` (`SourceView.fs:221-233`). The boundary
conversions the view does own — `tryFloat`, `radToDeg`, and constructing
`Polarization.create (Angle.degree d)` for the `SetPolarization` message — are forced by
the frozen `SourceMsg` shapes (that case takes a `Polarization`, not a float), so they
belong at the view boundary. Responsibilities sit where they should.

## Consistency

The local `SourceViewMsg = Edit | ApplyPreset` (`SourceView.fs:54-56`) mirrors the
materials panel's view-local `MaterialsMsg` precedent precisely, and the SoW/impl-log
both justify why presets cannot fold into `SourceMsg` (`applyPreset` writes `coherence`,
which no `SourceMsg` case expresses — verified at `PolarizationPicker.fs:46-59` and
`SourceEditorView.fs:45-53`). Module header doc, `*View.fs` naming, fsproj registration
with an explanatory comment, the `RootMsg.Source` wiring, and the `initFrom` seed from
`defaultSource "source-1"` all match the established pattern. The `unitButton`
active-highlight is a nice touch with no preset-side equivalent, but presets are
stateless actions so that asymmetry is defensible.

## Spec fit

Tight, no scope creep, no under-delivery. All six `SourceMsg` cases are surfaced (the
five numeric/azimuth fields plus the five-way unit selector at `SourceView.fs:125-145`);
all six presets are offered (`SourceView.fs:174-179`); ellipse → AvaPlot and Poincaré →
WebView2, never the reverse, satisfying the Non-requirement. AC-U4.1 (field edit routes
`SourceEditorView.update`, readout refreshes) and AC-U4.2 (preset refreshes ellipse +
Poincaré) each have a dedicated test. §0.4 is vacuously satisfied — this slice adds no
effectful `Cmd` (`RootMsg.Source` returns `Cmd.none`) — and §0.5 holds: `source :
SourceSpec` is pure, the live controls are host-layer locals inside the adapters.

## Evolvability

The root model holds a single `source : SourceSpec` field (`Shell.fs:76`). When the
deferred §E.8 multi-source list lands, that field must become a list + active-selection,
and `SourcePanelTests`' `(fst Shell.init).source` reference moves with it. This is a
localized reshape confined to the new-from-this-arc `Shell.fs` and the test — not a
forced cross-tree change and not a cornering — but the next author should expect it. The
`SourceViewMsg` union is a clean extension point for the bidirectional-drag write-back
(`setAzimuthEllipticity` already exists, `PolarizationPicker.fs:70`), so that future
slice has a seam waiting.

## Risks

- **Routing assertion couples to placeholder text (low).** The AC-U4.2 routing test
  asserts exactly one `"Plotly chart (WebView2)"` text node
  (`SourcePanelTests.fs:115-116`), keyed off the literal in `ChartHosts.unavailable`
  (`ChartHosts.fs:103`). It is the only headless-observable way to prove the ellipse is
  not in WebView2, so the choice is reasonable, but a future edit to the placeholder
  string will silently break this test rather than the feature. A shared constant would
  harden it; not required this slice.
- **One "view" test is a pure-update test (minor).** `a source field edit routes
  SourceEditorView.update...` (`SourcePanelTests.fs:71-83`) carries the `ui-tests` trait
  but never mounts a window — it exercises `SourceView.update` directly. The comment
  explains headless text edits cannot be raised, and `ChartPanelTests` set the same
  precedent, so this is consistent; just note it asserts delegation, not rendering.
- **Reformat-on-render (cosmetic).** `numberField` re-sets `TextBox.text` via
  `sprintf "%g"` each render (`SourceView.fs:97`); a successful parse that reformats the
  entry (e.g. trailing zeros) can move the caret. Partial/blank entries are guarded
  (`tryFloat` → no dispatch → no re-render), so the model is never corrupted; this is a
  pre-existing field-editor UX trait, not a defect introduced here.

## Bottom line

I would ship this. It is a render-and-wire slice that honours the freeze mandate, reuses
every existing seam, and matches the surrounding panels' shape down to the message-union
idiom. The notes above are hardening suggestions, not corrections — none rise to a
re-spawn. The judge should weigh the all-green gate roster (build 0 errors; berreman 84,
constructor 204, ui-smoke 1, ui-tests 13) against these minor, non-blocking observations.
