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
