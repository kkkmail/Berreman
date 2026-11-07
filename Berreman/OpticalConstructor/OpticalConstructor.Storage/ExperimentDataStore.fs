namespace OpticalConstructor.Storage

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ExperimentData

/// Reads measured data through its kit; reading a picked file's bytes is its only direct IO
/// (spec 0038 step 036; spec 0050 S4.2).
[<RequireQualifiedAccess>]
module ExperimentDataStore =

    /// Which measured-data file a load reads, as its failure names it.
    type private DataFileKind =
        | IntensityDataFile
        | EllipsometricDataFile

        member this.text : string =
            match this with
            | IntensityDataFile -> "intensity"
            | EllipsometricDataFile -> "ellipsometric"

    let private loadThrough
        (kit : KitProxy)
        (parse : DataFileText -> Result<'Series, ExperimentDataError>)
        (kind : DataFileKind)
        (attachment : DataAttachment)
        : Result<'Series, ExperimentDataError> =
        match kit.readDataFile attachment.path with
        | Error error -> Error (MalformedDataFileErr $"could not read the {kind.text} data file '{attachment.path.value}': {error.describe}")
        | Ok file -> parse file.text.dataFileText

    /// Attaches picked files and parses measured data through the supplied kit (spec 0050 S4.2).
    let createFileBacked (kit : KitProxy) : ExperimentDataProxy =
        {
            attachDataFile =
                fun file ->
                    let read =
                        try Ok (FileByteContent (File.ReadAllBytes file.value))
                        with error -> Error (KitIoErr $"could not read the picked data file '{file.value}': {error.Message}")
                    read
                    |> Result.bind (fun content ->
                        let source : LabFileSource =
                            {
                                file = file
                                content = content
                            }
                        kit.attachLabFile KitDataFolder.root source)
            attachmentStatus = kit.attachmentStatus
            tryLoadIntensity = loadThrough kit (fun text -> text.parseIntensitySeries) IntensityDataFile
            tryLoadEllipsometric = loadThrough kit (fun text -> text.parseEllipsometricSeries) EllipsometricDataFile
        }
