# 016 — Spec 0022 "Optical Constructor" arc-runner: completion summary

## Arc-runner outcome

The spec-0022 arc completed **green** on branch `0022`: **16 / 16 slices**
committed, all `manual: false` (no hand-accepts), final log line `COMPLETE`,
exit code `0`.

It took three launches to get there — the first two stops were
infrastructure, not the implementation:

1. **Launch 1 — escalated at boot (exit 4).** Pre-loop health-check failed:
   all 16 per-slice `.gates` snapshots had been written by the splitter in
   the bare-list short form (`gates: [build, unit-tests,
   constructor-unit-tests]`), which the arc-runner's gate-descriptor reader
   rejects. Fixed by hand — see
   [015](./015-per-slice-gates-snapshots-must-be-full-descriptors.md).
2. **Launch 2 — escalated at slice 015 (exit 5).** Reason
   `max-wall-time-exceeded (37205s > 36000s)`: the arc hit its built-in
   10-hour wall-clock cap at a slice boundary. Slices 001–014 were already
   committed green. Resumable.
3. **Launch 3 — COMPLETE (exit 0).** Resumed from slice 015 (state file
   present), finished 015 + 016 in ~1h06m. Fresh 10-hour budget.

**Commit SHAs (slice → sha):**
001 `bc1dac1` · 002 `ac140e6` · 003 `f293dff` · 004 `df6feb7` ·
005 `5386001` · 006 `87e23d3` · 007 `fdfa97e` · 008 `6540add` ·
009 `e8a2bac` · 010 `9bca676` · 011 `99db5bd` · 012 `710db28` ·
013 `c56dde7` · 014 `56328e0` · 015 `f64b67c` · 016 `50794902`.

Final slice 016 ("UX shell III: job runner, help/gallery, 3D system view")
passed code-judge `done-green` on review cycle 2 and committed 24 files
(*23 changed, +1989/−18*).

Gate roster on every slice: `build`, `unit-tests`, `constructor-unit-tests`
— all green throughout. The hand-repaired gate snapshots (015) held across
all 16 slices.

Artifacts: per-slice impl-plan / impl-log / state-of-the-world under
`specs/0022/.slices/`; critic + judge transcripts under
`specs/0022/.artifacts/`.

---

## Q1 — What tests were added for the new functionality?

**Framework:** xUnit **v3** (`xunit.v3` 3.2.2, `coverlet.collector` 10.0.1),
targeting `net10.0` / x64, in the new project
`Berreman/OpticalConstructor/OpticalConstructor.Tests/`.

**Totals (from `016-state-of-the-world.md` / `016-impl-log.md`):**

| gate | passing | note |
|------|---------|------|
| `constructor-unit-tests` | **204** | the new Optical Constructor suite (this spec) |
| `unit-tests` (legacy BerremanTests) | 84 (+5 skipped) | pre-existing solver tests, untouched |

The 204 constructor tests are spread over ~31 `*.fs` files, one (or more)
per spec part. Highlights of what they cover:

- **Domain / physics core** — units & boundary conversions
  (`UnitsTests`, eV/wavelength/wavenumber round-trips), gradient-index
  auto-discretization (`GradientDiscretizeTests`), beam-tree topology &
  mirror/branch rules (`BeamTreeTests`), curved elements
  (`CurvedElementsTests`), beam routing through the reused Berreman engine
  seams (`BeamRoutingTests`), dispersion models & anisotropy
  (`DispersionModelsTests`).
- **Storage / persistence** — canonical-JSON round-trip + schema validation
  (`ProjectJsonRoundtripTests`, `RoundTripTests` for `.ocproj`), `.binz`
  sidecar derived-artefact round-trip (`SidecarTests`), material
  import/export from refractiveindex.info (`MaterialImportTests`),
  undo/redo + autosave (`HistoryTests`), CSV/material/design-history export
  (`ExportImportTests`), environment/preferences persistence
  (`EnvironmentRoundTripTests`).
- **Editors & sources** — stack/layer editor transforms incl. drag-drop
  material assignment (`StackEditTests`, 16 facts), source editor
  projection & expand-on-boundary (`SourceProjectionTests`,
  `SourceExpansionTests`).
- **Optimization / fitting (Part G)** — `OptimizationInterfaceTests`
  (Levenberg–Marquardt / Nelder–Mead interfaces), `LocalRefinementTests`,
  `SynthesisFitPageTests` (fit report, χ², confidence intervals, cancel).
- **Charts / readout (Part H)** — `SeriesDataTests`, `ReadoutTests`,
  `ChartSettingsTests`, `PolarizationPlotTests`.
- **UX shell (Part J, slices 014–016)** — `SchematicGeometryTests`
  (cross-section geometry **AC-J1** + the slice-016 **AC-J12** 3D
  `SystemView3D` geometry), `RepeatBuilderTests` (AC-J2),
  `TemplatesTests` (AC-J3, six stack templates), `ValidationTests` (AC-J9),
  **`JobRunnerCancelTests`** (AC-J10, slice 016 — background job harness,
  progress ticks, cancel semantics, sidecar artefact confinement),
  **`HelpGalleryTests`** (AC-J11, slice 016 — glossary / sample-project
  gallery, eV·cm⁻¹ conversions reuse `Domain.Units`).

**The three tests added by the final slice (016):** `JobRunnerCancelTests`
(8 facts) + `HelpGalleryTests` (7 facts) + the AC-J12 additions to
`SchematicGeometryTests` (4 facts) = the 19 new facts that took the
constructor suite from 185 → 204.

Design note: the UI-bearing parts are tested through **pure projection /
geometry / MVU-transform functions** (e.g. schematic geometry, beam
directions, job-runner progress math, MVU messages), so the suite runs
headless under the `constructor-unit-tests` gate with no Avalonia render
host — this is the §A.6 / P3 testability mandate.

---

## Q2 — Decision on `C:\GitHub\Avalonia.FuncUI.Clone\`

**Decision: the clone is deliberately NOT referenced, linked, or built.**
The app builds against the **public, MIT-licensed official NuGet package
`Avalonia.FuncUI` 1.6.0** instead.

Why: the spec (binding constraint 5 / §A.9, and **AC-A8**) requires the
clone to pass a mandatory audit and be cleared before anything may
reference, link, or build against it — and it explicitly leaves the
*linking mechanism* (local NuGet vs. ProjectReference) **undecided** until
that audit decision is taken later. As of this arc the **audit is
NOT-YET-RUN** (per `001-impl-log.md`), so the clone stays unreferenced and
the linking choice unresolved.

How the UI dependency is actually incorporated — `PackageReference` only,
in `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`:

```xml
<!-- Public, MIT-licensed Avalonia + FuncUI NuGet (the buildable interpretation
     of §A.6). This is NOT the audit-gated clone at Avalonia.FuncUI.Clone,
     which stays unreferenced until the §A.9 audit passes (AC-A8). -->
<PackageReference Include="Avalonia"               Version="11.3.4" />
<PackageReference Include="Avalonia.Desktop"       Version="11.3.4" />
<PackageReference Include="Avalonia.Themes.Fluent" Version="11.3.4" />
<PackageReference Include="Avalonia.FuncUI"        Version="1.6.0" />
<PackageReference Include="ScottPlot.Avalonia"     Version="5.1.58" />
<PackageReference Include="Plotly.NET"             Version="5.1.0" />
<PackageReference Include="OpenTK"                 Version="4.9.4" />
<PackageReference Include="Microsoft.Web.WebView2" Version="1.0.3967.48" />
```

There is **no `ProjectReference` to `C:\GitHub\Avalonia.FuncUI.Clone\`**
anywhere in the solution. The UI shell is authored against the FuncUI DSL
*surface*, so swapping in the audited clone later (if/when the audit
passes) is a dependency-substitution, not a rewrite.

**Open item for you:** the §A.9 clone audit still has to be run, and the
link-mechanism decision (NuGet vs. ProjectReference into the clone) is
still owed. Until then the app ships on the public NuGet FuncUI.

---

## Q3 — Was an installer created? How do I run it?

**No installer was created** — and that is per spec, not an omission. The
spec defines no packaging/deployment scope (constraint 6, "minimum
implementation"); there are no Inno Setup (`.iss`), WiX (`.wixproj`),
MSIX, `.nuspec`, publish-profile (`.pubxml`), or single-file/self-contained
publish directives in the repo, and no CI workflow that builds one.

**So yes — you just build and run it from source.** The runnable app is the
desktop executable project **`OpticalConstructor.App`** (`OutputType=Exe`,
`net10.0`, x64; `Program.fs` entry point), which references the
`OpticalConstructor.Ui` library.

From the repo root `C:\GitHub\Berreman`:

```powershell
# 1. Build (Release) — same command the `build` gate runs
dotnet build Berreman/Berreman.slnx -c Release

# 2a. Run via dotnet
dotnet run --project Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj -c Release

# 2b. …or launch the built exe directly
#     Berreman/OpticalConstructor/OpticalConstructor.App/bin/Release/net10.0/<app>.exe
```

**Prerequisites:** the **.NET 10 SDK/runtime** on Windows x64. UI stack is
Avalonia 11.3.4 desktop + Avalonia.FuncUI 1.6.0, with ScottPlot (2D
charts), Plotly.NET via WebView2 (3D charts), and OpenTK (3D viewport) —
all restored automatically from public NuGet on first build. The 3D /
chart panels assume the WebView2 runtime is present (standard on current
Windows 11).

If you want a double-click distributable later, that's a follow-up:
`dotnet publish -c Release -r win-x64 --self-contained` (optionally
`-p:PublishSingleFile=true`), or a real installer — none of which this spec
asked for.
