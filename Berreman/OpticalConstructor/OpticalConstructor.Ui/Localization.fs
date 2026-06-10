/// EN/RU localization (Spec 0026 Part I, slice 003). A centralized, runtime-read,
/// human-editable string resource and the loader/completeness machinery around it.
/// This is net-new (constraint 0.7): there is no `CultureInfo`-based localization,
/// `ResourceManager`, or `.resx` in the solution, and none is introduced here.
///
/// The translatable strings live in ONE file, `strings.json` (I.1.1), shipped beside
/// the assembly as a build-copied content item and read at run time from
/// `AppContext.BaseDirectory` — mirroring the env-schema / project-schema load
/// (`SchemaValidation.fs:17`, `UserEnvironment.schemaFullPath`). An operator or
/// translator can edit the strings, or add a further language, by editing that file:
/// the loader reads it at run time, so NO rebuild is required.
///
/// Lookup falls back to the English entry when a string is missing for the active
/// language, then to the key itself, so a missing translation never crashes and never
/// shows blank (I.3.1). `check` reports the keys missing for the active language and
/// renders a single copyable message the app entry point surfaces (`Program.fs`).
///
/// Scientific symbols (Ψ, Δ, °, R1/R2/R3, the unit symbols) are kept OUT of the
/// translatable resource entirely (`Symbols`), so they stay language-neutral by
/// construction and a translation pass cannot localize them (I.4.1).
module OpticalConstructor.Ui.Localization

open System
open System.IO
open System.Text.Json

// ---------------------------------------------------------------------------
// Language (I.2) — the persisted/restored selection; a field of EnvironmentSettings.
// ---------------------------------------------------------------------------

/// The selectable UI language (I.2.1). A fieldless DU, so the shared
/// `ProjectJson.options` serialize it as a bare string ("English"/"Russian") and the
/// environment schema constrains it with an `enum`. EN and RU ship; the `strings.json`
/// mechanism supports adding a further language by editing the file (no third language
/// ships — constraint 0.6).
type Language =
    | English
    | Russian

/// The resource's language code for `language` — the per-entry property name in
/// `strings.json` (`"en"` / `"ru"`).
let languageCode (language : Language) : string =
    match language with
    | English -> "en"
    | Russian -> "ru"

/// A human-readable language name for the copyable completeness message (I.3.1).
let languageName (language : Language) : string =
    match language with
    | English -> "English"
    | Russian -> "Russian"

// ---------------------------------------------------------------------------
// The parsed resource + error channel.
// ---------------------------------------------------------------------------

/// A loaded string resource (I.1.1): each key maps to a per-language-code value map,
/// e.g. `"app.title" -> { "en" -> "Optical Constructor"; "ru" -> "..." }`. Pure and
/// serializable; carries no Avalonia handle (constraint 0.3).
type Resource =
    {
        entries : Map<string, Map<string, string>>
    }

/// Why a resource load failed. Surfaced — never thrown — so the app entry point can
/// render a copyable message rather than crash (I.3.1).
type LocalizationError =
    | ResourceMissing of path : string
    | ResourceParseError of message : string

/// The outcome of the startup completeness check (I.3.1): the keys that had no value
/// for the active language (they fall back to English), plus a single copyable
/// message naming them (empty when the resource is complete).
type CompletenessReport =
    {
        language : Language
        missingKeys : string list
        message : string
    }

// ---------------------------------------------------------------------------
// Parsing & loading (I.1.1) — runtime-read, build-copied beside the assembly.
// ---------------------------------------------------------------------------

/// The shipped resource file name (I.1.1). Copied beside the assembly by the
/// `OpticalConstructor.Ui.fsproj` content item, mirroring the env schema.
[<Literal>]
let resourceFileName = "strings.json"

/// The runtime resource path: `strings.json` beside the assembly. Read from
/// `AppContext.BaseDirectory`, exactly as the env schema is loaded
/// (`UserEnvironment.schemaFullPath`) — the SAME build-copied-content-item +
/// base-directory pattern (`SchemaValidation.fs:17`, I.5).
let resourcePath () : string =
    Path.Combine(AppContext.BaseDirectory, resourceFileName)

/// Parse the resource JSON `{ key: { "en": ..., "ru": ... } }` into a `Resource`.
/// Non-object entries / non-string values are skipped rather than throwing, so a
/// partially malformed file still yields the entries it can (the completeness check
/// then reports whatever is missing). A non-object ROOT is a parse error.
let parse (json : string) : Result<Resource, LocalizationError> =
    try
        use doc = JsonDocument.Parse json
        let root = doc.RootElement
        if root.ValueKind <> JsonValueKind.Object then
            Error (ResourceParseError "the strings resource root must be a JSON object of key -> { language: value } entries")
        else
            let entries =
                [ for prop in root.EnumerateObject() ->
                    let byLang =
                        if prop.Value.ValueKind = JsonValueKind.Object then
                            [ for langProp in prop.Value.EnumerateObject() do
                                if langProp.Value.ValueKind = JsonValueKind.String then
                                    yield langProp.Name, langProp.Value.GetString() ]
                            |> Map.ofList
                        else
                            Map.empty
                    prop.Name, byLang ]
                |> Map.ofList
            Ok { entries = entries }
    with e ->
        Error (ResourceParseError e.Message)

/// Load and parse the resource at `path`. A missing file or read/parse failure is
/// returned, never thrown (I.3.1 — the entry point renders it copyably instead of
/// crashing).
let loadFromFile (path : string) : Result<Resource, LocalizationError> =
    if File.Exists path then
        try parse (File.ReadAllText path)
        with e -> Error (ResourceParseError e.Message)
    else
        Error (ResourceMissing path)

// ---------------------------------------------------------------------------
// Lookup with English fallback (I.3.1).
// ---------------------------------------------------------------------------

/// Resolve `key` for the active `language` (I.1.1 / I.3.1). When the active-language
/// value is absent or empty, fall back to the English entry; when English is also
/// absent, fall back to the key itself — so a missing string is visible and
/// debuggable but never crashes or shows blank.
let lookup (resource : Resource) (language : Language) (key : string) : string =
    match Map.tryFind key resource.entries with
    | None -> key
    | Some byLang ->
        match Map.tryFind (languageCode language) byLang with
        | Some v when v <> "" -> v
        | _ ->
            match Map.tryFind (languageCode English) byLang with
            | Some v when v <> "" -> v
            | _ -> key

// ---------------------------------------------------------------------------
// Completeness check + copyable message (I.3.1).
// ---------------------------------------------------------------------------

/// Check the resource for completeness against the active `language` (I.3.1): every
/// key that lacks a non-empty value for `language` is reported (it falls back to
/// English at lookup) and named in a single, copyable message. A complete resource
/// yields an empty key list and an empty message (never a false alarm).
let check (resource : Resource) (language : Language) : CompletenessReport =
    let code = languageCode language
    let missingKeys =
        resource.entries
        |> Map.toList
        |> List.choose (fun (key, byLang) ->
            match Map.tryFind code byLang with
            | Some v when v <> "" -> None
            | _ -> Some key)
        |> List.sort
    let message =
        if List.isEmpty missingKeys then ""
        else
            let header =
                sprintf
                    "Localization: %d interface string(s) are missing for the selected language (%s). The English text is shown instead. Missing keys:"
                    (List.length missingKeys) (languageName language)
            let lines = missingKeys |> List.map (sprintf "  - %s")
            String.concat "\n" (header :: lines)
    { language = language; missingKeys = missingKeys; message = message }

/// Render a load failure as a copyable message (I.3.1) — surfaced when the resource is
/// missing or malformed, again without crashing.
let describeError (err : LocalizationError) : string =
    match err with
    | ResourceMissing path ->
        sprintf
            "Localization: the string resource '%s' was not found at %s. Interface text falls back to the embedded keys."
            resourceFileName path
    | ResourceParseError message ->
        sprintf "Localization: the string resource '%s' could not be read: %s" resourceFileName message

/// The startup completeness check the app entry point wires (I.3.1 / `Program.fs`):
/// load the shipped resource and return a copyable error message when it is missing,
/// malformed, or incomplete for `language`; `None` when it loads and is complete. The
/// app still runs with the English fallback in every case (never crash, never silent).
let startupCheck (language : Language) : string option =
    match loadFromFile (resourcePath ()) with
    | Error err -> Some (describeError err)
    | Ok resource ->
        let report = check resource language
        if List.isEmpty report.missingKeys then None else Some report.message

// ---------------------------------------------------------------------------
// Language-neutral scientific symbols (I.4.1).
// ---------------------------------------------------------------------------

/// Scientific symbols that MUST stay language-neutral and MUST NOT be translated
/// (I.4.1): Ψ, Δ, °, R1/R2/R3, and the unit symbols. These are compiled constants —
/// deliberately NOT entries in the translatable `strings.json` — so a translation
/// pass cannot localize them. `neutralSymbol` resolves them ignoring the language,
/// making the language-neutral property explicit and testable.
module Symbols =
    /// Ellipsometric & rotation symbols.
    let psi = "Ψ"        // Ψ
    let delta = "Δ"      // Δ
    let degree = "°"     // °
    let r1 = "R1"
    let r2 = "R2"
    let r3 = "R3"
    /// Unit symbols — the language-neutral display glyphs for `UnitOfMeasure`.
    let meter = "m"
    let millimeter = "mm"
    let micrometer = "µm"      // µm
    let nanometer = "nm"
    let angstrom = "Å"         // Å
    let electronVolt = "eV"
    let wavenumber = "cm⁻¹" // cm⁻¹

    /// The full no-translate set (I.4.1) — every glyph that stays identical in every
    /// language and is never read from the translatable resource.
    let neutral =
        [ psi; delta; degree; r1; r2; r3
          meter; millimeter; micrometer; nanometer; angstrom; electronVolt; wavenumber ]

/// Resolve a language-neutral scientific symbol (I.4.1). It ignores `language` by
/// design: the same glyph is returned for every language and it is never read from
/// the translatable `strings.json`, so the symbol cannot be localized.
let neutralSymbol (_language : Language) (glyph : string) : string = glyph
