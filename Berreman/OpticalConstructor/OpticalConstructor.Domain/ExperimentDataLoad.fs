namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Library                 // LibraryProxy / DetectorKind / DetectorItem
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Placement               // CatalogueKind.Detector
open OpticalConstructor.Domain.Setups                  // SetupSlot
open OpticalConstructor.Domain.MeasuredData            // MeasuredSeries / validateAgainstExperiment / ExperimentDataError
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy

/// Loads a kit attachment in the shape of the detector the experiment's setup slots hold and validates its range,
/// through injected proxies (spec 0038 Part L step 037; spec 0050 S4.2, S8.2).
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

    /// Loads the kit attachment in the shape of the detector among the experiment's resolved `slots` and validates the
    /// series against the experiment, preserving typed errors (spec 0038 Part L step 037; spec 0050 S4.2, S8.2).
    let loadAndValidate
        (library : LibraryProxy)
        (dataProxy : ExperimentDataProxy)
        (slots : SetupSlot list)
        (experiment : Experiment)
        (attachment : DataAttachment)
        : Result<MeasuredSeries, ExperimentDataError> =
        let detector = detectorKindOf library slots
        let varied = experiment.varied |> Option.map (fun v -> v.variable)
        match detector with
        | Intensity ->
            dataProxy.tryLoadIntensity attachment
            |> Result.map IntensityData
            |> Result.bind (validateAgainstExperiment varied experiment.range detector)
        | Ellipsometer ->
            dataProxy.tryLoadEllipsometric attachment
            |> Result.map EllipsometricData
            |> Result.bind (validateAgainstExperiment varied experiment.range detector)
