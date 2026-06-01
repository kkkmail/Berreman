namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors

/// Recent-files persistence + autosave (§I.5 / R-1). Both go through the slice-003
/// storage core: the recent-files preferences travel the shared `ProjectJson.options`
/// JSON stack (NO second serializer), and autosave reuses `ProjectFile.saveProject`
/// so it writes the SAME canonical JSON the user's `.ocproj` does — never a `.binz`
/// pickle (binding rule 4). The recent-files list is the discoverability surface the
/// Operator-facing UX checklist requires; autosave keeps a distinct `.autosave` file
/// off the committed document so a write never clobbers the user's `.ocproj`.
module RecentFiles =

    /// Bound on the most-recently-used list (§I.5 — "bounded list"). The UI renders
    /// the list under File ▸ Recent (Part J); the depth policy beyond this cap is
    /// the UI's, not this module's.
    [<Literal>]
    let maxRecent = 16

    /// The single per-user preferences file (§I.5 — "a single JSON preferences file
    /// in the per-user application-data folder"). This is the product's own designed
    /// app-data path (not a worker run-artifact), so it legitimately lives under
    /// `ApplicationData`.
    let private prefsPath () =
        let dir =
            Path.Combine(
                Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
                "Softellect", "Berreman", "OpticalConstructor")
        Path.Combine(dir, "recent.json")

    /// Pure MRU update: drop any case-insensitive duplicate of `path`, push it to the
    /// front, and truncate to `maxRecent` (most-recent first). Kept separate from the
    /// IO so the list invariant is testable without touching the app-data file.
    let bump (path : string) (existing : string list) : string list =
        existing
        |> List.filter (fun p -> not (String.Equals(p, path, StringComparison.OrdinalIgnoreCase)))
        |> fun kept -> path :: kept
        |> List.truncate maxRecent

    /// Load the most-recently-used `.ocproj` paths (§I.5). A missing or unreadable
    /// preferences file yields the empty list rather than throwing — `loadRecent`'s
    /// signature is total (`unit -> string list`), so the UI never has to handle an
    /// error case just to render an empty Recent menu.
    let loadRecent () : string list =
        try
            let p = prefsPath ()
            if File.Exists p then
                JsonSerializer.Deserialize<string list>(File.ReadAllText p, ProjectJson.options)
            else []
        with _ -> []

    /// Push `path` onto the recent list and persist it (§I.5), returning the updated
    /// list. Persistence is best-effort: a failed write does not lose the returned
    /// in-memory list (retry/rolling generations are out of scope, §I.5).
    let pushRecent (path : string) : string list =
        let updated = bump path (loadRecent ())
        try
            let p = prefsPath ()
            Directory.CreateDirectory(Path.GetDirectoryName p) |> ignore
            File.WriteAllText(p, JsonSerializer.Serialize(updated, ProjectJson.options))
        with _ -> ()
        updated

/// Autosave path derivation + write (§I.5 / R-1). The autosave timer interval and
/// trigger are owned by the UI model (Part J §J.10); this module owns only the
/// path derivation and the write function.
module Autosave =

    /// The autosave sidewrite suffix (§I.5). Appended to the canonical `.ocproj`
    /// path so the autosave file is a distinct sibling (`<name>.ocproj.autosave`),
    /// contributed to `.gitignore` and never committed.
    [<Literal>]
    let autosaveSuffix = ".autosave"

    /// Derive the sibling autosave path for a project file path (§I.5). For
    /// `…/foo.ocproj` this is `…/foo.ocproj.autosave`.
    let autosavePath (name : string) : string = name + autosaveSuffix

    /// Write the project to its `.autosave` sidewrite path (§I.5), reusing
    /// `ProjectFile.saveProject` (and so `serializeProject` — the same canonical
    /// JSON). It writes ONLY the autosave file and MUST NOT overwrite the user's
    /// `.ocproj` (AC-I6); a failed write returns `FileIoError` for the UI to surface.
    let writeAutosave (name : string) (project : OpticalConstructorProject) : Result<unit, StorageError> =
        ProjectFile.saveProject (autosavePath name) project
