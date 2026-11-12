namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Library                 // LibraryProxy / DetectorKind / DetectorItem
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles       // ContentHash
open OpticalConstructor.Domain.Placement               // CatalogueKind.Detector
open OpticalConstructor.Domain.Setups                  // SetupSlot
open OpticalConstructor.Domain.MeasuredData            // MeasuredPointCount / ExperimentDataError
open OpticalConstructor.Domain.ImportProfiles          // MappedPoints
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy

/// Loads a kit attachment through its import profile and validates its points against the detector the experiment's setup
/// slots hold and its scan, through injected proxies (spec 0038 Part L step 037; spec 0050 S4.2, S8.2, S9.1, S9.2).
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

    /// Reads the attachment's points through the proxy, its import profile and its row selection, and validates the
    /// points its exclusions keep against the detector among the resolved `slots` and the experiment's scan (spec 0038
    /// Part L step 037; spec 0050 S4.2, S8.2, S9.1, S9.2, S9.3).
    let loadAndValidate
        (library : LibraryProxy)
        (dataProxy : ExperimentDataProxy)
        (slots : SetupSlot list)
        (experiment : Experiment)
        (data : MappedAttachment)
        : Result<MappedPoints, ExperimentDataError> =
        dataProxy.readMapped data
        |> Result.bind (fun points -> (points.keptBy data.exclusions).validatedFor (detectorKindOf library slots) experiment.scan)

    /// How many of an attachment's points were validated and how many its exclusions name (spec 0050 S9.3).
    type KeptPointsInfo =
        {
            kept : MeasuredPointCount
            excluded : MeasuredPointCount
        }

    /// An experiment's data status: no data, mapped, validated with its points, validated with excluded points, changed
    /// since attach or out of range (spec 0050 S9.1, S9.3; 001 §K, §E).
    type ExperimentDataBadge =
        | NoDataBadge
        /// Mapped but not validated, with the reason when the file did not read (spec 0050 S9.1).
        | MappedDataBadge of problemOpt : ExperimentDataError option
        | ValidatedDataBadge of points : MeasuredPointCount
        /// Validated over the kept points while some are excluded (spec 0050 S9.3).
        | ExcludedPointsBadge of counts : KeptPointsInfo
        /// The file's hash now differs from the attachment's, or `None` when the file is missing (spec 0050 S9.1).
        | DataChangedBadge of attachment : DataAttachment * currentOpt : ContentHash option
        /// A range or units mismatch against the experiment (spec 0050 S9.1).
        | OutOfRangeBadge of problem : ExperimentDataError

        member this.text : string =
            match this with
            | NoDataBadge -> "no data"
            | MappedDataBadge _ -> "mapped"
            | ValidatedDataBadge points -> $"validated ({points.value} points)"
            | ExcludedPointsBadge counts -> $"excluded points ({counts.excluded.value} excluded, {counts.kept.value} validated)"
            | DataChangedBadge _ -> "data changed"
            | OutOfRangeBadge _ -> "out of range"

        /// The one rule: no data; else a changed or missing file, ahead of any outcome; else the validation's outcome,
        /// counting the excluded points beside the validated ones, or merely mapped without one (spec 0050 S9.1, S9.3).
        static member ofCheck
            (dataOpt : MappedAttachment option)
            (statusOpt : DataAttachmentStatus option)
            (outcomeOpt : Result<MappedPoints, ExperimentDataError> option)
            : ExperimentDataBadge =
            match dataOpt, statusOpt, outcomeOpt with
            | None, _, _ -> NoDataBadge
            | Some data, Some (AttachedDataChanged current), _ -> DataChangedBadge (data.attachment, Some current)
            | Some data, Some AttachedDataMissing, _ -> DataChangedBadge (data.attachment, None)
            | Some data, _, Some (Ok points) ->
                match data.exclusions with
                | [] -> ValidatedDataBadge points.pointCount
                | exclusions -> ExcludedPointsBadge { kept = points.pointCount; excluded = MeasuredPointCount exclusions.Length }
            | Some _, _, Some (Error (DataRangeMismatchErr _ as problem))
            | Some _, _, Some (Error (DataUnitsMismatchErr _ as problem)) -> OutOfRangeBadge problem
            | Some _, _, Some (Error problem) -> MappedDataBadge (Some problem)
            | Some _, _, None -> MappedDataBadge None
