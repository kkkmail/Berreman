namespace OpticalConstructor.Storage

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Storage.LabFileImport

/// Reads an attachment through its kit, the kit file's import profiles and its row selection; reading a picked file's
/// bytes is its only direct IO (spec 0038 step 036; spec 0050 S4.2, S9.2).
[<RequireQualifiedAccess>]
module ExperimentDataStore =

    type FileName with

        /// The picked file's absolute name and bytes, read whole, or `KitIoErr` (spec 0050 S4.2, S9.2).
        member private this.pickedLabFile : Result<LabFileSource, KitError> =
            try
                let source : LabFileSource =
                    {
                        file = this
                        content = FileByteContent (File.ReadAllBytes this.value)
                    }
                Ok source
            with error -> Error (KitIoErr $"could not read the picked data file '{this.value}': {error.Message}")

    /// Reads and attaches picked files, and reads attachments through their import profiles, through the supplied kit
    /// (spec 0050 S4.2, S9.2).
    let createFileBacked (kit : KitProxy) : ExperimentDataProxy =
        {
            readPickedFile = fun file -> file.pickedLabFile
            attachDataFile = fun file -> file.pickedLabFile |> Result.bind (kit.attachLabFile KitDataFolder.root)
            attachmentStatus = kit.attachmentStatus
            readMapped =
                fun (data : MappedAttachment) ->
                    let unread (error : KitError) : ExperimentDataError =
                        MalformedDataFileErr
                            $"could not read the data file '{data.attachment.path.value}' for the import profile '{data.profile.value}': {error.describe}"

                    kit.readKitFile ()
                    |> Result.mapError unread
                    |> Result.bind (fun kitFile ->
                        match ChosenImportProfile.tryOfName kitFile.importProfiles data.profile with
                        | None -> Error (MalformedDataFileErr $"the import profile '{data.profile.value}' is not available")
                        | Some profile ->
                            kit.readDataFile data.attachment.path
                            |> Result.mapError unread
                            |> Result.bind (fun file -> profile.mapText file.text)
                            |> Result.bind (fun points -> points.selectedBy data.rows))
        }
