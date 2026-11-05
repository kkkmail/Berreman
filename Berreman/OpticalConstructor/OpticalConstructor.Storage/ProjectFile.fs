namespace OpticalConstructor.Storage

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.ProjectJson

/// `.ocproj` project file read/write (§I.3 / R-3) — the single project-open seam
/// every later part routes template and gallery loads through (Part J §J.3,
/// §J.11). The canonical project is a single self-contained UTF-8 JSON text file;
/// it is NOT a zip, folder bundle, or multi-file container. All filesystem access
/// is wrapped so any exception becomes `FileIoExn`, never propagating out.
module ProjectFile =

    /// The canonical project file extension (§I.3). `.ocproj` files are user
    /// documents and are never added to `.gitignore`.
    [<Literal>]
    let extension = ".ocproj"

    type FileName with

        /// Serialize via `OpticalConstructorProject.serializeProject` and write the UTF-8 text here.
        member this.saveProject (project : OpticalConstructorProject) : Result<unit, StorageError> =
            match project.serializeProject with
            | Error e -> Error e
            | Ok json ->
                try
                    File.WriteAllText(this.value, json.value, System.Text.UTF8Encoding(false))
                    Ok()
                with e -> Error(FileIoExn e)

        /// Read the file here, then `parseProject` so schema validation always runs on
        /// load (§I.3). Returns the validated, bound project or a `StorageError`.
        member this.openProject () : Result<OpticalConstructorProject, StorageError> =
            let read =
                try Ok(ProjectJsonText (File.ReadAllText this.value))
                with e -> Error(FileIoExn e)

            match read with
            | Error e -> Error e
            | Ok json -> json.parseProject
