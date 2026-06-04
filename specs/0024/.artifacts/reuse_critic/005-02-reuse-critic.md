# Reuse critique -- 005.slice-md cycle 1

## Coverage

- Helper roots walked: `Berreman/OpticalConstructor/OpticalConstructor.Ui/` (sibling
  `*View.fs`, `Shell.fs`, `Charts/`, `Sources/`), `Berreman/Berreman/` (`Geometry.fs`,
  `Fields.fs`, math backends), `Berreman/OpticalConstructor/OpticalConstructor.Storage/`.
- Files inspected: ~24/200.
- Extensions: the task's declared filter (`.py,.md,.json`) is a generic template that does
  NOT match this pure-F# project; the actual helper roots are `.fs`. I walked `.fs` sources
  as the evidence base (the defensible read per "pick the most defensible interpretation").
- Diff read from `git diff HEAD` stdout (new `SourceView.fs`; `Shell.fs`, two `.fsproj` edits).

## Findings

### F1: `plotRow` is a verbatim copy of `ChartView.chartRow`

- **Worker added:** `SourceView.plotRow` (`OpticalConstructor.Ui/SourceView.fs:236`).
- **Existing helper:** `ChartView.chartRow`
  (`OpticalConstructor.Ui/ChartView.fs:163`).
- **Why it matters:** The two are identical line-for-line — same `StackPanel` (vertical,
  `spacing 2.0`, `margin 4.0`), same bold-title `TextBlock`, same `Border.height 220.0`
  wrapping the host — and the worker even carried over the doc comment ("One titled,
  fixed-height *chart*/*plot* row so each hosted plot lays out a frame"). This is direct
  duplication of the slice-002 titled-plot-row idiom. Worse, the `220.0` fixed plot height
  is now a magic constant living in two places: a future change to plot-row chrome (height,
  spacing, a header affordance) has to be made twice and will silently drift. The slice's
  own Architecture note says it "reuses the slice-002 `ChartHosts` adapters rather than
  embedding their own hosting" — but the *row scaffolding* around those adapters was copied,
  not reused.
- **Suggested action:** Extract one shared titled-plot-row helper both panels call. `chartRow`
  is currently `private` to `ChartView`, so the lift is a small one (e.g. a public
  `ChartHosts.plotRow title host` next to the adapters it wraps, since every caller pairs it
  with `scottPlotHost`/`webView2Host`), after which `ChartView` and `SourceView` both delegate.
  Advisory; the judge decides routing.

### F2: `radToDeg` re-derives the shared `degree` conversion constant

- **Worker added:** `SourceView.radToDeg r = r * 180.0 / System.Math.PI`
  (`OpticalConstructor.Ui/SourceView.fs:81`), used to display `incidenceAngle` and
  `polarization` in degrees (`SourceView.fs:138`, `SourceView.fs:140`).
- **Existing helper:** the project's canonical radian↔degree seam — the `degree` constant
  `Constants.Pi / 180.0` (`Berreman/MathNetNumericsMath.fs:11`, dup at
  `ExtremeNumericsMath.fs:10`) and `Angle.degrees = a / degree`
  (`Berreman/Geometry.fs:38`). Every angle-to-degree conversion in the engine goes through
  this: `IncidenceAngle.description` is `this.value / degree` (`Berreman/Fields.fs:32`),
  `Polarization.description` likewise (`Fields.fs:325`).
- **Why it matters:** The worker hand-rolled a second definition of the degree conversion
  (`* 180.0 / π` is exactly `/ degree`) instead of routing the one the engine already uses
  everywhere. It is numerically equivalent today, but it is a second source of truth for a
  unit conversion in a numerical-optics codebase whose whole discipline is single-seam unit
  handling — exactly the kind of divergence the engine factored out into `degree`/`Angle`.
- **Suggested action:** Convert through the existing seam — `s.light.incidenceAngle.value /
  Berreman.Geometry.degree` (the idiom the `description` members use), or reconstruct an
  `Angle` and read `.degrees`. `Berreman.Geometry` is already `open`ed in `SourceView.fs:29`,
  so the constant is in reach. Advisory.

### F3: `tryFloat` is a third private copy of the invariant-culture float parser

- **Worker added:** `SourceView.tryFloat` (`OpticalConstructor.Ui/SourceView.fs:73`) — an
  `InvariantCulture` `Double.TryParse → float option`.
- **Existing helper:** the same private parser already exists twice in the Storage layer:
  `SpectralImport.tryFloat` (`OpticalConstructor.Storage/SpectralImport.fs:27`) and
  `MaterialImport.tryFloat` (`OpticalConstructor.Storage/MaterialImport.fs:36`), both
  `Double.TryParse(s.Trim(), NumberStyles.Float, InvariantCulture)`.
- **Why it matters:** This is the third copy of "parse a user/file scalar in invariant
  culture, `None` on garbage" in `OpticalConstructor`. The worker's variant adds
  `AllowThousands` and skips the `Trim()`, so it is a *near-miss* rather than an exact dup —
  but the divergence is the problem: three slightly different float parsers means three
  slightly different notions of what a valid number is at the app's input boundaries.
- **Suggested action:** This one is genuinely marginal. The two existing copies are `private`
  inside the **frozen** Storage modules (§0.1 forbids editing them), and the layering
  (UI parse vs CSV-import parse) is a defensible reason to keep them separate. The honest call
  is either to leave `SourceView.tryFloat` as-is and document the duplication is intentional,
  or — if a shared `Parse`/`Units` util is ever introduced — to fold all three into it. No
  action is strictly required for this slice; flagged so the third copy is a conscious choice,
  not an accident. Advisory.

## Bottom line

F1 is the substantive finding: a verbatim copy of `ChartView.chartRow`, including a baked-in
`220.0` magic height, in a slice whose stated discipline is "reuse the slice-002 hosting,"
and it is a clean extract-to-shared-helper. F2 is a real but smaller single-seam divergence
(a re-derived `degree` conversion) that a numerical codebase should not carry twice; F3 is a
near-miss I would not re-spawn over on its own. My read: F1 alone is a defensible re-spawn
trigger if the judge wants the row helper shared before the panel count grows; F2+F3 are
better folded into that same touch than chased separately. No gate authority — the code judge
decides.
