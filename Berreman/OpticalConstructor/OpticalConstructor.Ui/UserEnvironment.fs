/// §J.6 — persistent UI-customization & favorites board [Core] + §J.7 preferences
/// [Standard] (010 Part II §1/§4/§8). `EnvironmentSettings` persists the user's
/// working environment between sessions and seeds defaults for new projects: a
/// named/grouped favorites board (pinned reused engine `Layer`/`OpticalSystem`
/// fragments and `materialEntry`/`sourceSpec` ids — §A.7 shapes, by value), the
/// last working folder(s) and the ordered recent-files list, the window/panel
/// layout and theme (J.8), the toolbar contents, and the `Preferences` (J.7).
///
/// Persistence is JSON validated on load against `optical-constructor-environment.
/// schema.json` (added to the §A.7 schema family, same `JsonSchema.Net` library) —
/// NOT FsPickler/`.binz`, because it is hand/LLM-inspectable config (binding rule
/// 4 / §A.7). The serialization REUSES the slice-003 shared `ProjectJson.options`
/// `JsonSerializerOptions` (§I.1) — never a second JSON stack — so the favorites'
/// `Layer`/`OpticalSystem` (and their `Matrix<Complex>` tensors) round-trip through
/// the same converters the canonical project does. Schema versioning/migration is
/// OUT OF SCOPE (§0 constraint 6): on any validation failure `load` falls back to
/// the built-in `defaults` rather than attempt a migration (AC-J6).
///
/// Solver defaults are the engine's OWN `SolverParameters` (`Solvers.fs:15`, whose
/// `numberOfReflections` is the convergence control); Part J defines NO parallel
/// solver-config record (R-2/§J.7/AC-J7). Default-unit and wavelength-range
/// preferences are display/boundary concerns: they live here as labels/canonical-SI
/// magnitudes and are applied only at the UI/IO boundary, never changing how a
/// stored value is represented.
module OpticalConstructor.Ui.UserEnvironment

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Json.Schema
open Berreman.Constants
open Berreman.Media
open Berreman.Solvers
open OpticalConstructor.Domain.Units
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors

// ---------------------------------------------------------------------------
// Theme & dockable-panel layout (J.8 fields of EnvironmentSettings).
// ---------------------------------------------------------------------------

/// Light/dark theme selection (§J.8). A fieldless DU serialized as a bare string
/// by the shared options' `WithUnionUnwrapFieldlessTags`; mapped to an Avalonia
/// `ThemeVariant` by `AppShell` (the only Avalonia-aware seam).
type Theme =
    | Light
    | Dark

/// A dock edge for a panel (§J.8). Fieldless DU → bare string. `Center` is the
/// fill region (the chart/results work area).
type DockSide =
    | Left
    | Right
    | Top
    | Bottom
    | Center

/// One dockable/resizable panel's saved state (§J.8): its id, its dock edge, its
/// size along the dock axis (device-independent pixels), and whether it is shown.
type PanelState =
    {
        panel : string
        dock : DockSide
        size : float
        visible : bool
    }

/// The serialized window/panel layout (§J.8) — the ordered set of dockable panels.
/// A field of `EnvironmentSettings`, round-tripping through its JSON (AC-J8).
type PanelLayout =
    {
        panels : PanelState list
    }

// ---------------------------------------------------------------------------
// Preferences (§J.7).
// ---------------------------------------------------------------------------

/// A default wavelength range (§J.7). Endpoints are stored in the canonical SI
/// base (meters; §0 constraint 3) — the `wavelengthUnit` preference is only the
/// display label applied at the boundary, never a stored non-SI datum.
type WavelengthRange =
    {
        min : float<meter>
        max : float<meter>
    }

/// User preferences (§J.7, [Standard]). Default per-quantity units, the default
/// wavelength range, the default sweep-point count, the decimal display precision,
/// and the solver defaults. Solver defaults are the engine's own `SolverParameters`
/// (`Solvers.fs:15`) — Part J introduces NO parallel solver-config record (AC-J7).
/// Persists as part of `EnvironmentSettings` (J.6), hence JSON not `.binz`.
type Preferences =
    {
        /// Default unit for length/thickness boundary fields (a display/IO label).
        lengthUnit : UnitOfMeasure
        /// Default unit for wavelength boundary fields (a display/IO label).
        wavelengthUnit : UnitOfMeasure
        /// Default wavelength range for new sweeps (canonical SI meters).
        wavelengthRange : WavelengthRange
        /// Default number of sweep points.
        sweepPoints : int
        /// Decimal places for display formatting.
        decimalPrecision : int
        /// Solver defaults — the engine's `SolverParameters` (Solvers.fs:15).
        solver : SolverParameters
    }

// ---------------------------------------------------------------------------
// Favorites board (§J.6 [Core]).
// ---------------------------------------------------------------------------

/// One pinned favorite (§J.6). Reused-engine `Layer`/`OpticalSystem` fragments are
/// stored BY VALUE (so a pinned favorite survives even if the project it came from
/// is deleted), while library materials and sources are referenced by their stable
/// `materialEntry`/`sourceSpec` ids (§A.7). Serialized by the shared options as the
/// `{ "Case", "Fields" }` adjacent-tag shape.
type FavoritePin =
    | LayerPin of Layer
    | SystemPin of OpticalSystem
    | MaterialPin of string
    | SourcePin of string

/// A named, grouped collection of pins (§J.6 — "named, grouped pins").
type FavoriteGroup =
    {
        name : string
        pins : FavoritePin list
    }

// ---------------------------------------------------------------------------
// The persistent environment (§J.6 root record).
// ---------------------------------------------------------------------------

/// The persistent user environment (§J.6 [Core] + J.7/J.8 fields). Persisted as
/// schema-validated JSON (NOT `.binz`); the favorites board, recent files, last
/// folders, panel layout, theme, toolbar, and preferences all round-trip through
/// it (AC-J6/AC-J8). This is the net-new persistent customization from 010 Part
/// II §1.
type EnvironmentSettings =
    {
        favorites : FavoriteGroup list
        lastFolders : string list
        recentFiles : string list
        layout : PanelLayout
        theme : Theme
        /// Chart color palettes surfaced to Part H (J.8); hex color strings. The
        /// palette IMPLEMENTATION is Part H — this only carries the configured set.
        chartPalette : string list
        toolbar : string list
        preferences : Preferences
    }

// ---------------------------------------------------------------------------
// Built-in defaults (§J.6 item 3 — the fall-back on validation failure).
// ---------------------------------------------------------------------------

/// The built-in default panel arrangement: stack/materials/sources docked left,
/// the chart in the fill region, results docked right.
let defaultPanels : PanelState list =
    [
        { panel = "stack";     dock = Left;   size = 240.0; visible = true }
        { panel = "materials"; dock = Left;   size = 240.0; visible = true }
        { panel = "sources";   dock = Left;   size = 240.0; visible = true }
        { panel = "chart";     dock = Center; size = 600.0; visible = true }
        { panel = "results";   dock = Right;  size = 320.0; visible = true }
    ]

/// Built-in default preferences: nanometer length/wavelength labels, a 400–800 nm
/// visible range (canonical meters), 100 sweep points, 4-digit display, and the
/// engine's own default `SolverParameters` (Solvers.fs:15).
let defaultPreferences : Preferences =
    {
        lengthUnit = Nanometer
        wavelengthUnit = Nanometer
        wavelengthRange = { min = 400.0e-9<meter>; max = 800.0e-9<meter> }
        sweepPoints = 100
        decimalPrecision = 4
        solver = SolverParameters.defaultValue
    }

/// The built-in default environment — the fall-back the app uses when no settings
/// file exists or one fails schema validation (AC-J6, no migration).
let defaults : EnvironmentSettings =
    {
        favorites = []
        lastFolders = []
        recentFiles = []
        layout = { panels = defaultPanels }
        theme = Light
        chartPalette = [ "#1f77b4"; "#ff7f0e"; "#2ca02c"; "#d62728"; "#9467bd" ]
        toolbar = [ "open"; "save"; "build"; "solve"; "chart" ]
        preferences = defaultPreferences
    }

// ---------------------------------------------------------------------------
// JSON persistence (§J.6 item 1) — schema-validated, shared options, never .binz.
// ---------------------------------------------------------------------------

/// The single shipped env-schema version, pinned with `const` in the schema. A
/// differing value fails validation and so falls back to defaults (no migration).
[<Literal>]
let schemaVersion = "1.0"

/// The build-copied env schema content item, loaded beside the assembly at runtime
/// (the fsproj copies it to the output root). Mirrors the slice-003 project-schema
/// load.
let schemaFileName = "optical-constructor-environment.schema.json"

let private schemaFullPath =
    Path.Combine(AppContext.BaseDirectory, schemaFileName)

/// Load the published env schema ONCE behind `lazy` — `JsonSchema.FromText`
/// registers the document by its (distinct) `$id` in the process-global registry,
/// so a single load keeps validate-on-load idempotent across calls.
let private envSchema : Result<JsonSchema, StorageError> Lazy =
    lazy
        if File.Exists schemaFullPath then
            try File.ReadAllText schemaFullPath |> JsonSchema.FromText |> Ok
            with e -> Error (FileIoError e)
        else
            Error SchemaMissing

/// Validate a parsed document against the env schema (§J.6 item 1) via
/// `JsonSchema.Net`. On any evaluation failure returns `SchemaValidationError`
/// with the library's messages; the caller MUST NOT bind on `Error`.
let validate (element : JsonElement) : Result<unit, StorageError> =
    match envSchema.Value with
    | Error e -> Error e
    | Ok schema ->
        let options = EvaluationOptions(OutputFormat = OutputFormat.List)
        let results = schema.Evaluate(element, options)
        if results.IsValid then Ok ()
        else
            let messages =
                match results.Errors with
                | null -> []
                | errs -> [ for kv in errs -> $"{results.InstanceLocation}: {kv.Key} -> {kv.Value}" ]
            let messages = if List.isEmpty messages then [ "environment schema validation failed" ] else messages
            Error (SchemaValidationError messages)

/// Serialize the environment to canonical JSON (§J.6 item 1), injecting the
/// envelope `schemaVersion`, REUSING the slice-003 shared `ProjectJson.options`
/// (§I.1) — never a second JSON stack, never `.binz`.
let serialize (settings : EnvironmentSettings) : Result<string, StorageError> =
    try
        let node = JsonSerializer.SerializeToNode(settings, ProjectJson.options)
        let envelope = node.AsObject()
        envelope.["schemaVersion"] <- JsonValue.Create(schemaVersion)
        Ok(envelope.ToJsonString(ProjectJson.options))
    with e -> Error(JsonParseError e.Message)

/// Parse, validate-on-load, then bind (§J.6 item 1). Schema validation ALWAYS runs
/// before the record is admitted; a `schemaVersion` mismatch fails the schema's
/// `const` and surfaces as `SchemaValidationError`, never a migration (AC-J6).
let deserialize (json : string) : Result<EnvironmentSettings, StorageError> =
    let parsed =
        try Ok(JsonDocument.Parse json)
        with e -> Error(JsonParseError e.Message)

    match parsed with
    | Error e -> Error e
    | Ok doc ->
        use doc = doc
        match validate doc.RootElement with
        | Error e -> Error e
        | Ok () ->
            try Ok(doc.RootElement.Deserialize<EnvironmentSettings>(ProjectJson.options))
            with e -> Error(JsonParseError e.Message)

// ---------------------------------------------------------------------------
// Fixed per-user path + total load / save (§J.6 item 2).
// ---------------------------------------------------------------------------

/// The single fixed per-user settings path under the OS application-data folder
/// (§J.6 item 2 — outside the repository working tree, so a committable-vs-gitignore
/// non-issue). Mirrors the slice-013 recent-files path convention.
let settingsPath () : string =
    let dir =
        Path.Combine(
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
            "Softellect", "Berreman", "OpticalConstructor")
    Path.Combine(dir, "environment.json")

/// Load the environment from `path`, falling back to the built-in `defaults` on ANY
/// failure — missing file, malformed JSON, or schema-validation failure (AC-J6, no
/// migration). Total `path -> EnvironmentSettings`, so the UI never has to handle an
/// error case just to render a usable environment.
let load (path : string) : EnvironmentSettings =
    try
        if File.Exists path then
            match deserialize (File.ReadAllText path) with
            | Ok s -> s
            | Error _ -> defaults
        else defaults
    with _ -> defaults

/// Persist the environment to `path` as schema-validated canonical JSON (§J.6 item
/// 1). A serialization or write failure returns `StorageError` for the UI to
/// surface; it never throws across this boundary.
let save (path : string) (settings : EnvironmentSettings) : Result<unit, StorageError> =
    match serialize settings with
    | Error e -> Error e
    | Ok json ->
        try
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, json)
            Ok()
        with e -> Error(FileIoError e)
