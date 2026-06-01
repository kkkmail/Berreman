namespace OpticalConstructor.Storage

open System.IO
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open OpticalConstructor.Storage.Errors

/// `.binz` sidecar read/write (§I.4 / R-4) for DERIVED/BULK artefacts ONLY —
/// parametric-sweep result tables (`Variables.fs:237,270`), in-layer field maps,
/// and fit/design histories. It reuses `Softellect.Sys.Core.serialize`/
/// `deserialize` with `BinaryZippedFormat` (`Core.fs:254,257`); no second pickler,
/// no second zip path, no custom binary format. The canonical project definition
/// MUST NOT travel this path (binding rule 4) — `writeSidecar` of an
/// `OpticalConstructorProject` is forbidden by directive and never done.
module Sidecar =

    /// The sidecar file extension, taken from the Softellect format itself
    /// (`Primitives.fs:267`) rather than hand-written, so it cannot drift.
    let extension =
        let (FileExtension e) = BinaryZippedFormat.fileExtension
        e

    /// The single sidecar-location seam (§I.4 / §J.10-item-6 / §G.10-item-6): the
    /// `.sidecars` subfolder beside a project's own directory where ALL derived/bulk
    /// `.binz` artefacts live — sweep/field-map results AND fit/design histories —
    /// NEVER the repository root. Every derived-artefact writer routes through here so
    /// the flat-vs-subfolder choice is decided once (reuse finding F2 of the cycle-1
    /// review): `JobRunner.derivedArtefactPath` (sweeps/fits) and
    /// `SynthesisFitPage.fitHistorySidecarPath` (Part G fit history) both call it.
    let sidecarDirectory (projectDir : string) : string =
        Path.Combine(projectDir, ".sidecars")

    /// A derived-artefact `.binz` path under `sidecarDirectory projectDir`, appending
    /// the drift-proof `extension` so the `.binz` suffix is never hand-written. The
    /// path is ALWAYS under the project's `.sidecars` folder, never the repo root.
    let derivedArtefactPath (projectDir : string) (name : string) : string =
        Path.Combine(sidecarDirectory projectDir, name + extension)

    /// Persist a derived artefact `value` as a zipped-FsPickler `.binz` sidecar.
    let writeSidecar<'T> (path : string) (value : 'T) : Result<unit, StorageError> =
        try
            let bytes = serialize BinaryZippedFormat value
            File.WriteAllBytes(path, bytes)
            Ok()
        with e -> Error(BinzError e)

    /// Read a `.binz` sidecar back to its value. A missing file surfaces as
    /// `SidecarMissing` (§I.4 / AC-I5) rather than throwing; a corrupt/incompatible
    /// payload surfaces as `BinzError`.
    let readSidecar<'T> (path : string) : Result<'T, StorageError> =
        if not (File.Exists path) then
            Error(SidecarMissing path)
        else
            try
                let bytes = File.ReadAllBytes path
                Ok(deserialize BinaryZippedFormat bytes : 'T)
            with e -> Error(BinzError e)
