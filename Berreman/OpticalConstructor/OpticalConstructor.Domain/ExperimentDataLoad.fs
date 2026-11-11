namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Library                 // LibraryProxy / DetectorKind / DetectorItem
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles       // ContentHash
open OpticalConstructor.Domain.Placement               // CatalogueKind.Detector
open OpticalConstructor.Domain.Setups                  // SetupSlot
open OpticalConstructor.Domain.MeasuredData            // MeasuredSeries / validateAgainstExperiment / ExperimentDataError
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy

/// Loads a kit attachment through its import profile and validates it against the detector the experiment's setup slots
/// hold and its range, through injected proxies (spec 0038 Part L step 037; spec 0050 S4.2, S8.2, S9.1).
module ExperimentDataLoad =

    /// The detector kind the first `Detector` slot's binding resolves to through the Library, which fixes the expected
    /// data-file shape; `Intensity` when no slot is a detector or its binding resolves to no detector, as the live
    /// scene's `runDetectorKind` defaults (spec 0038 Part L; spec 0050 S8.2).
    let detectorKindOf (library : LibraryProxy) (slots : SetupSlot list) : DetectorKind =
        match slots |> List.tryFind (fun slot -> slot.placement.catalogueKind = Detector) with
        | Some slot ->
            match slot.placement.valueId with
            | Some entryId ->
                match library.tryGetEntry entryId with
                | Ok (Some (DetectorItem detector)) -> detector.kind
                | Ok (Some _) | Ok None | Error _ -> Intensity
            | None -> Intensity
        | None -> Intensity

    type DetectorKind with

        /// The built-in import profile a file attached under this detector is read through (spec 0050 S9.1).
        member this.builtInProfile : ImportProfileName =
            match this with
            | Intensity -> ImportProfileName.intensitySeries
            | Ellipsometer -> ImportProfileName.ellipsometricSeries

    /// Reads the mapped file through its import profile and validates it against the detector among the resolved `slots`
    /// and the scan's one axis, or a dark line's R1 default range. An unavailable profile, a key selection or any other
    /// scan reads nothing and is refused typed: an X,Y file holds one variable and no key column (spec 0038 Part L step
    /// 037; spec 0050 S4.2, S8.2, S9.1).
    let loadAndValidate
        (library : LibraryProxy)
        (dataProxy : ExperimentDataProxy)
        (slots : SetupSlot list)
        (experiment : Experiment)
        (data : MappedAttachment)
        : Result<MeasuredSeries, ExperimentDataError> =
        let readByProfile () : Result<MeasuredSeries, ExperimentDataError> =
            let readerOpt : (DataAttachment -> Result<MeasuredSeries, ExperimentDataError>) option =
                if data.profile = ImportProfileName.intensitySeries then
                    Some (dataProxy.tryLoadIntensity >> Result.map IntensityData)
                elif data.profile = ImportProfileName.ellipsometricSeries then
                    Some (dataProxy.tryLoadEllipsometric >> Result.map EllipsometricData)
                else
                    None
            match readerOpt, data.rows with
            | None, _ -> Error (MalformedDataFileErr $"the import profile '{data.profile.value}' is not available")
            | Some _, KeyedDataRows key ->
                Error (MalformedDataFileErr $"{data.attachment.path.value}: an X,Y data file has no key column to select '{key.value}' by")
            | Some read, AllDataRows -> read data.attachment

        let validateOver (varied : VariableParameter option) (range : VariableRange) : Result<MeasuredSeries, ExperimentDataError> =
            readByProfile () |> Result.bind (validateAgainstExperiment varied range (detectorKindOf library slots))

        match experiment.scan.blocks with
        | [ { axes = [ axis ] } ] -> validateOver (Some axis.variable) axis.range
        | [ { axes = [] } ] -> validateOver None (VariableRange.forVariable VaryR1)
        | blocks ->
            Error (DataRangeMismatchErr $"an X,Y data file holds one variable, and the scan varies {experiment.scan.axes.Length} in {blocks.Length} block(s)")

    /// An experiment's data status: no data, mapped, validated with its points, changed since attach or out of range
    /// (spec 0050 S9.1; 001 §K, §E).
    type ExperimentDataBadge =
        | NoDataBadge
        /// Mapped but not validated, with the reason when the file did not read (spec 0050 S9.1).
        | MappedDataBadge of problemOpt : ExperimentDataError option
        | ValidatedDataBadge of points : MeasuredPointCount
        /// The file's hash now differs from the attachment's, or `None` when the file is missing (spec 0050 S9.1).
        | DataChangedBadge of attachment : DataAttachment * currentOpt : ContentHash option
        /// A range or units mismatch against the experiment (spec 0050 S9.1).
        | OutOfRangeBadge of problem : ExperimentDataError

        member this.text : string =
            match this with
            | NoDataBadge -> "no data"
            | MappedDataBadge _ -> "mapped"
            | ValidatedDataBadge points -> $"validated ({points.value} points)"
            | DataChangedBadge _ -> "data changed"
            | OutOfRangeBadge _ -> "out of range"

        /// The one rule: no data; else a changed or missing file, ahead of any outcome; else the validation's outcome, or
        /// merely mapped without one (spec 0050 S9.1).
        static member ofCheck
            (dataOpt : MappedAttachment option)
            (statusOpt : DataAttachmentStatus option)
            (outcomeOpt : Result<MeasuredSeries, ExperimentDataError> option)
            : ExperimentDataBadge =
            match dataOpt, statusOpt, outcomeOpt with
            | None, _, _ -> NoDataBadge
            | Some data, Some (AttachedDataChanged current), _ -> DataChangedBadge (data.attachment, Some current)
            | Some data, Some AttachedDataMissing, _ -> DataChangedBadge (data.attachment, None)
            | Some _, _, Some (Ok series) -> ValidatedDataBadge series.pointCount
            | Some _, _, Some (Error (DataRangeMismatchErr _ as problem))
            | Some _, _, Some (Error (DataUnitsMismatchErr _ as problem)) -> OutOfRangeBadge problem
            | Some _, _, Some (Error problem) -> MappedDataBadge (Some problem)
            | Some _, _, None -> MappedDataBadge None
