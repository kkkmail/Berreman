namespace OpticalConstructor.Storage

open System.IO
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors

/// `.ocproj` project file read/write (§I.3 / R-3) — the single project-open seam
/// every later part routes template and gallery loads through (Part J §J.3,
/// §J.11). The canonical project is a single self-contained UTF-8 JSON text file;
/// it is NOT a zip, folder bundle, or multi-file container. All filesystem access
/// is wrapped so any exception becomes `FileIoError`, never propagating out.
module ProjectFile =

    /// The canonical project file extension (§I.3). `.ocproj` files are user
    /// documents and are never added to `.gitignore`.
    [<Literal>]
    let extension = ".ocproj"

    /// Serialize via `ProjectJson.serializeProject` and write UTF-8 text to `path`.
    let saveProject (path : string) (project : OpticalConstructorProject) : Result<unit, StorageError> =
        match ProjectJson.serializeProject project with
        | Error e -> Error e
        | Ok json ->
            try
                File.WriteAllText(path, json, System.Text.UTF8Encoding(false))
                Ok()
            with e -> Error(FileIoError e)

    /// Read `path`, then `deserializeProject` so schema validation always runs on
    /// load (§I.3). Returns the validated, bound project or a `StorageError`.
    let openProject (path : string) : Result<OpticalConstructorProject, StorageError> =
        let read =
            try Ok(File.ReadAllText path)
            with e -> Error(FileIoError e)

        match read with
        | Error e -> Error e
        | Ok json -> ProjectJson.deserializeProject json
