namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Library                 // LibraryProxy / DetectorKind / DetectorItem
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData            // MeasuredSeries / validateAgainstExperiment / ExperimentDataError
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy

/// Loads a kit attachment in the experiment's detector shape and validates its range through injected proxies
/// (spec 0038 Part L step 037; spec 0050 S4.2).
module ExperimentDataLoad =

    /// Resolve an experiment's detector kind through the Library (spec 034/037): the captured setup's
    /// detector descriptor binds a protected preset by its entry id (`PresetIds.detectorIntensity` /
    /// `PresetIds.detectorEllipsometer`), which resolves through the read-only `LibraryProxy` to the
    /// `DetectorKind` that fixes the expected data-file shape. Defaults to `Intensity` when the setup
    /// captured no detector, its binding is not a preset id, or the id does not resolve — mirroring the
    /// live scene's `runDetectorKind` default.
    let detectorKindOf (library : LibraryProxy) (experiment : Experiment) : DetectorKind =
        match experiment.detectorDescriptorOpt with
        | Some descriptor ->
            match descriptor.binding with
            | BoundByEntryId entryId ->
                match library.tryGetEntry entryId with
                | Ok (Some (DetectorItem detector)) -> detector.kind
                | Ok (Some _) | Ok None | Error _ -> Intensity
            | BoundByVersion _ | Unbound -> Intensity
        | None -> Intensity

    /// Loads the kit attachment and validates the series against the experiment, preserving typed errors
    /// (spec 0038 Part L step 037; spec 0050 S4.2).
    let loadAndValidate
        (library : LibraryProxy)
        (dataProxy : ExperimentDataProxy)
        (experiment : Experiment)
        (attachment : DataAttachment)
        : Result<MeasuredSeries, ExperimentDataError> =
        let detector = detectorKindOf library experiment
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
