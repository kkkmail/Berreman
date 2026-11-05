namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.ProjectFile

/// Recent-files persistence + autosave (§I.5 / R-1). Both go through the slice-003
/// storage core: the recent-files preferences travel the shared `CanonicalJson.options`
/// JSON stack (NO second serializer), and autosave reuses `FileName.saveProject`
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
    let private prefsPath () : FileName =
        let dir =
            Path.Combine(
                Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
                "Softellect", "Berreman", "OpticalConstructor")
            |> FolderName
        (FileName "recent.json").combine dir

    /// Pure MRU update: drop any case-insensitive duplicate of `path`, push it to the
    /// front, and truncate to `maxRecent` (most-recent first). Kept separate from the
    /// IO so the list invariant is testable without touching the app-data file.
    let bump (path : FileName) (existing : FileName list) : FileName list =
        existing
        |> List.filter (fun p -> not (String.Equals(p.value, path.value, StringComparison.OrdinalIgnoreCase)))
        |> fun kept -> path :: kept
        |> List.truncate maxRecent

    /// Load the most-recently-used `.ocproj` paths (§I.5). A missing or unreadable
    /// preferences file yields the empty list rather than throwing — `loadRecent`'s
    /// signature is total (`unit -> FileName list`), so the UI never has to handle an
    /// error case just to render an empty Recent menu. The file stays a JSON array of path strings.
    let loadRecent () : FileName list =
        try
            let p = prefsPath ()
            if File.Exists p.value then
                JsonSerializer.Deserialize<string list>(File.ReadAllText p.value, CanonicalJson.options)
                |> List.map FileName
            else []
        with _ -> []

    type FileName with

        /// Push this path onto the recent list and persist it (§I.5), returning the updated
        /// list. Persistence is best-effort: a failed write does not lose the returned
        /// in-memory list (retry/rolling generations are out of scope, §I.5).
        member this.pushRecent () : FileName list =
            let updated = bump this (loadRecent ())
            try
                let p = prefsPath ()
                Directory.CreateDirectory(Path.GetDirectoryName p.value) |> ignore
                File.WriteAllText(p.value, JsonSerializer.Serialize(updated |> List.map (fun f -> f.value), CanonicalJson.options))
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

    type FileName with

        /// The sibling autosave path of this project file path (§I.5). For
        /// `…/foo.ocproj` this is `…/foo.ocproj.autosave`.
        member this.autosavePath : FileName = this.addExtension (FileExtension autosaveSuffix)

        /// Write the project to this file's `.autosave` sidewrite path (§I.5), reusing
        /// `FileName.saveProject` (and so `serializeProject` — the same canonical
        /// JSON). It writes ONLY the autosave file and MUST NOT overwrite the user's
        /// `.ocproj` (AC-I6); a failed write returns `FileIoExn` for the UI to surface.
        member this.writeAutosave (project : OpticalConstructorProject) : Result<unit, StorageError> =
            this.autosavePath.saveProject project
