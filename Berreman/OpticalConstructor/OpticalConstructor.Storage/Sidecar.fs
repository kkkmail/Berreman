namespace OpticalConstructor.Storage

open System.IO
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open OpticalConstructor.Storage.Errors

/// `.binz` sidecars for derived/bulk artifacts only (§I.4 / R-4; spec 0050 S4.4).
/// Sweeps, field maps and fit histories reuse Softellect.Sys serialization with BinaryZippedFormat.
module Sidecar =

    /// The sidecar file extension, taken from the Softellect format itself
    /// (`Primitives.fs:267`) rather than hand-written, so it cannot drift.
    let sidecarExtension : FileExtension = BinaryZippedFormat.fileExtension

    type FolderName with

        /// The single sidecar-location seam (§I.4 / §J.10-item-6 / §G.10-item-6): the
        /// `.sidecars` subfolder of a project's own folder, where ALL derived/bulk
        /// `.binz` artefacts live — sweep/field-map results AND fit/design histories —
        /// NEVER the repository root. Every derived-artefact writer routes through here so
        /// the flat-vs-subfolder choice is decided once (reuse finding F2 of the cycle-1
        /// review): `derivedArtefactPath` (the §J.10 job artefacts: sweeps/fits) and
        /// `SynthesisFitPage.fitHistorySidecarPath` (Part G fit history) both reach it.
        member this.sidecarFolder : FolderName = this.combine (FolderName ".sidecars")

    /// A derived-artefact `.binz` path under `projectDir.sidecarFolder`, appending
    /// the drift-proof `sidecarExtension` so the `.binz` suffix is never hand-written. The
    /// path is ALWAYS under the project's `.sidecars` folder, never the repo root.
    let derivedArtefactPath (projectDir : FolderName) (name : FileName) : FileName =
        (name.addExtension sidecarExtension).combine projectDir.sidecarFolder

    type FileName with

        /// Persist a derived artefact `value` here as a zipped-FsPickler `.binz` sidecar.
        member this.writeSidecar<'Artefact> (value : 'Artefact) : Result<unit, StorageError> =
            try
                let bytes = serialize BinaryZippedFormat value
                File.WriteAllBytes(this.value, bytes.value)
                Ok()
            with e -> Error(BinzExn e)

        /// Read the `.binz` sidecar here back to its value. A missing file surfaces as
        /// `SidecarMissingErr` (§I.4 / AC-I5) rather than throwing; a corrupt/incompatible
        /// payload surfaces as `BinzExn`.
        member this.readSidecar<'Artefact> () : Result<'Artefact, StorageError> =
            if not (File.Exists this.value) then
                Error(SidecarMissingErr this)
            else
                try
                    let bytes = File.ReadAllBytes this.value
                    Ok(deserialize BinaryZippedFormat (SerializedData bytes) : 'Artefact)
                with e -> Error(BinzExn e)
