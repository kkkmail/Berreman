/// EN/RU localization (Spec 0026 Part I, slice 003). A centralized, runtime-read,
/// human-editable string resource and the loader/completeness machinery around it.
/// This is net-new (constraint 0.7): there is no `CultureInfo`-based localization,
/// `ResourceManager`, or `.resx` in the solution, and none is introduced here.
///
/// The translatable strings live in ONE file, `strings.json` (I.1.1), shipped beside
/// the assembly as a build-copied content item and read at run time from
/// `AppContext.BaseDirectory` — mirroring the env-schema / project-schema load
/// (`SchemaValidation.fs:17`, `UserEnvironment.schemaFileName`). An operator or
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
open Softellect.Sys.Primitives

// ---------------------------------------------------------------------------
// Language (I.2) — the persisted/restored selection; a field of EnvironmentSettings.
// ---------------------------------------------------------------------------

/// A per-entry property name in `strings.json` (`"en"` / `"ru"`).
type LanguageCode =
    | LanguageCode of string

    member this.value = let (LanguageCode c) = this in c

/// The selectable UI language (I.2.1). A fieldless DU, so the shared
/// `CanonicalJson.options` serialize it as a bare string ("English"/"Russian") and the
/// environment schema constrains it with an `enum`. EN and RU ship; the `strings.json`
/// mechanism supports adding a further language by editing the file (no third language
/// ships — constraint 0.6).
type Language =
    | English
    | Russian

    /// The resource's language code for this language.
    member this.code : LanguageCode =
        match this with
        | English -> LanguageCode "en"
        | Russian -> LanguageCode "ru"

    /// A human-readable language name for the copyable completeness message (I.3.1).
    member this.displayName : string =
        match this with
        | English -> "English"
        | Russian -> "Russian"

// ---------------------------------------------------------------------------
// The parsed resource + error channel.
// ---------------------------------------------------------------------------

/// One interface string as the resource gives it for one language.
type LocalizedText =
    | LocalizedText of string

    member this.value = let (LocalizedText t) = this in t

/// A key of the string resource, e.g. `"app.title"`.
type StringResourceKey =
    | StringResourceKey of string

    member this.value = let (StringResourceKey k) = this in k

    /// The key itself, shown when no language carries a value for it (I.3.1).
    member this.asFallbackText : LocalizedText = LocalizedText this.value

/// A loaded string resource (I.1.1): each key maps to a per-language-code value map,
/// e.g. `"app.title" -> { "en" -> "Optical Constructor"; "ru" -> "..." }`. Pure and
/// serializable; carries no Avalonia handle (constraint 0.3).
type Resource =
    {
        entries : Map<StringResourceKey, Map<LanguageCode, LocalizedText>>
    }

/// A copyable message the app entry point surfaces (I.3.1).
type LocalizationMessage =
    | LocalizationMessage of string

    member this.value = let (LocalizationMessage m) = this in m

/// The shipped resource file name (I.1.1). Copied beside the assembly by the
/// `OpticalConstructor.Ui.fsproj` content item, mirroring the env schema.
let resourceFileName : FileName = FileName "strings.json"

/// Why a resource load failed. Surfaced — never thrown — so the app entry point can
/// render a copyable message rather than crash (I.3.1).
type LocalizationError =
    | ResourceMissingErr of path : FileName
    | ResourceParseErr of message : string

    /// This load failure as a copyable message (I.3.1), again without crashing.
    member this.describe : LocalizationMessage =
        match this with
        | ResourceMissingErr path ->
            LocalizationMessage $"Localization: the string resource '%s{resourceFileName.value}' was not found at %s{path.value}. Interface text falls back to the embedded keys."
        | ResourceParseErr message ->
            LocalizationMessage $"Localization: the string resource '%s{resourceFileName.value}' could not be read: %s{message}"

/// The outcome of the startup completeness check (I.3.1): the keys that had no value
/// for the active language (they fall back to English), plus a single copyable
/// message naming them (empty when the resource is complete).
type CompletenessReport =
    {
        language : Language
        missingKeys : StringResourceKey list
        message : LocalizationMessage
    }

// ---------------------------------------------------------------------------
// Parsing & loading (I.1.1) — runtime-read, build-copied beside the assembly.
// ---------------------------------------------------------------------------

/// The text of a `strings.json` resource.
type StringResourceText =
    | StringResourceText of string

    member this.value = let (StringResourceText t) = this in t

    /// Parse the resource JSON `{ key: { "en": ..., "ru": ... } }` into a `Resource`.
    /// Non-object entries / non-string values are skipped rather than throwing, so a
    /// partially malformed file still yields the entries it can (the completeness check
    /// then reports whatever is missing). A non-object ROOT is a parse error.
    member this.parseResource : Result<Resource, LocalizationError> =
        try
            use doc = JsonDocument.Parse this.value
            let root = doc.RootElement
            if root.ValueKind <> JsonValueKind.Object then
                Error (ResourceParseErr "the strings resource root must be a JSON object of key -> { language: value } entries")
            else
                let entries =
                    [ for prop in root.EnumerateObject() ->
                        let byLang =
                            if prop.Value.ValueKind = JsonValueKind.Object then
                                [ for langProp in prop.Value.EnumerateObject() do
                                    if langProp.Value.ValueKind = JsonValueKind.String then
                                        yield LanguageCode langProp.Name, LocalizedText (langProp.Value.GetString()) ]
                                |> Map.ofList
                            else
                                Map.empty
                        StringResourceKey prop.Name, byLang ]
                    |> Map.ofList
                Ok { entries = entries }
        with e ->
            Error (ResourceParseErr e.Message)

type Resource with

    /// Load and parse the resource at `path`. A missing file or read/parse failure is
    /// returned, never thrown (I.3.1 — the entry point renders it copyably instead of
    /// crashing).
    static member tryLoad (path : FileName) : Result<Resource, LocalizationError> =
        if File.Exists path.value then
            try (StringResourceText (File.ReadAllText path.value)).parseResource
            with e -> Error (ResourceParseErr e.Message)
        else
            Error (ResourceMissingErr path)

/// The runtime resource path: `strings.json` beside the assembly. Read from
/// `AppContext.BaseDirectory`, exactly as the env schema is loaded
/// (`UserEnvironment.schemaFileName`) — the SAME build-copied-content-item +
/// base-directory pattern (`SchemaValidation.fs:17`, I.5).
let resourcePath () : FileName =
    resourceFileName.combine (FolderName AppContext.BaseDirectory)

// ---------------------------------------------------------------------------
// Lookup with English fallback (I.3.1).
// ---------------------------------------------------------------------------

/// Resolve `key` for the active `language` (I.1.1 / I.3.1). When the active-language
/// value is absent or empty, fall back to the English entry; when English is also
/// absent, fall back to the key itself — so a missing string is visible and
/// debuggable but never crashes or shows blank.
let lookup (resource : Resource) (language : Language) (key : StringResourceKey) : LocalizedText =
    match Map.tryFind key resource.entries with
    | None -> key.asFallbackText
    | Some byLang ->
        match Map.tryFind language.code byLang with
        | Some (LocalizedText v as text) when v <> "" -> text
        | _ ->
            match Map.tryFind English.code byLang with
            | Some (LocalizedText v as text) when v <> "" -> text
            | _ -> key.asFallbackText

// ---------------------------------------------------------------------------
// Completeness check + copyable message (I.3.1).
// ---------------------------------------------------------------------------

/// Check the resource for completeness against the active `language` (I.3.1): every
/// key that lacks a non-empty value for `language` is reported (it falls back to
/// English at lookup) and named in a single, copyable message. A complete resource
/// yields an empty key list and an empty message (never a false alarm).
let check (resource : Resource) (language : Language) : CompletenessReport =
    let missingKeys =
        resource.entries
        |> Map.toList
        |> List.choose (fun (key, byLang) ->
            match Map.tryFind language.code byLang with
            | Some (LocalizedText v) when v <> "" -> None
            | _ -> Some key)
        |> List.sort
    let message =
        match missingKeys with
        | [] -> LocalizationMessage ""
        | _ ->
            let header =
                $"Localization: %d{List.length missingKeys} interface string(s) are missing for the selected language (%s{language.displayName}). The English text is shown instead. Missing keys:"
            let lines = missingKeys |> List.map (fun k -> $"  - %s{k.value}")
            LocalizationMessage (String.concat "\n" (header :: lines))
    { language = language; missingKeys = missingKeys; message = message }

type Language with

    /// The startup completeness check the app entry point wires (I.3.1 / `Program.fs`):
    /// load the shipped resource and return a copyable error message when it is missing,
    /// malformed, or incomplete for this language; `None` when it loads and is complete. The
    /// app still runs with the English fallback in every case (never crash, never silent).
    member this.startupCheck () : LocalizationMessage option =
        match Resource.tryLoad (resourcePath ()) with
        | Error err -> Some err.describe
        | Ok resource ->
            let report = check resource this
            match report.missingKeys with
            | [] -> None
            | _ -> Some report.message

// ---------------------------------------------------------------------------
// Language-neutral scientific symbols (I.4.1).
// ---------------------------------------------------------------------------

/// A scientific glyph that stays identical in every language (I.4.1).
type NeutralSymbol =
    | NeutralSymbol of string

    member this.value = let (NeutralSymbol s) = this in s

/// Scientific symbols that MUST stay language-neutral and MUST NOT be translated
/// (I.4.1): Ψ, Δ, °, R1/R2/R3, and the unit symbols. These are compiled constants —
/// deliberately NOT entries in the translatable `strings.json` — so a translation
/// pass cannot localize them. `neutralSymbol` resolves them ignoring the language,
/// making the language-neutral property explicit and testable.
module Symbols =
    /// Ellipsometric & rotation symbols.
    let psi : NeutralSymbol = NeutralSymbol "Ψ"
    let delta : NeutralSymbol = NeutralSymbol "Δ"
    let degree : NeutralSymbol = NeutralSymbol "°"
    let r1 : NeutralSymbol = NeutralSymbol "R1"
    let r2 : NeutralSymbol = NeutralSymbol "R2"
    let r3 : NeutralSymbol = NeutralSymbol "R3"
    /// Unit symbols — the language-neutral display glyphs for `UnitOfMeasure`.
    let meter : NeutralSymbol = NeutralSymbol "m"
    let millimeter : NeutralSymbol = NeutralSymbol "mm"
    let micrometer : NeutralSymbol = NeutralSymbol "µm"
    let nanometer : NeutralSymbol = NeutralSymbol "nm"
    let angstrom : NeutralSymbol = NeutralSymbol "Å"
    let electronVolt : NeutralSymbol = NeutralSymbol "eV"
    let wavenumber : NeutralSymbol = NeutralSymbol "cm⁻¹"

    /// The full no-translate set (I.4.1) — every glyph that stays identical in every
    /// language and is never read from the translatable resource.
    let neutral : NeutralSymbol list =
        [ psi; delta; degree; r1; r2; r3
          meter; millimeter; micrometer; nanometer; angstrom; electronVolt; wavenumber ]

/// Resolve a language-neutral scientific symbol (I.4.1). It ignores `language` by
/// design: the same glyph is returned for every language and it is never read from
/// the translatable `strings.json`, so the symbol cannot be localized.
let neutralSymbol (_language : Language) (glyph : NeutralSymbol) : NeutralSymbol = glyph
