namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Library                 // LibraryProxy / DetectorKind / DetectorItem
open OpticalConstructor.Domain.Experiments             // Experiment / ElementBinding / DataFilePath
open OpticalConstructor.Domain.MeasuredData            // MeasuredSeries / validateAgainstExperiment / ExperimentDataError
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy

/// Spec 0038 Part L (step 037) — the pure orchestration that turns an experiment's attached data file
/// into a validated measured series through the injected proxies. It is the seam the inverse flow's
/// per-experiment attach status reads (and the future `SolverHandoffWindow`'s per-file validation
/// will reuse): resolve the experiment's detector kind through the Library, read the file at its
/// `DataFilePath` through the `ExperimentDataProxy` in the shape that detector kind fixes (intensity
/// or ellipsometric), and validate the parsed series against the step-25 experiment's varied
/// parameter and range. Pure given the proxies (the proxies are pure DATA — records of
/// `Result`-returning functions), so a test drives it end-to-end over in-memory mock proxies with no
/// real IO. No filesystem access of its own: the disk read lives behind the `ExperimentDataProxy`.
///
/// Compiles LAST in the Domain (after `ExperimentDataProxy.fs`): it names `Library` (the detector
/// resolution), `Experiments` (the experiment + its `DataFilePath`), `MeasuredData` (the series /
/// validation) and `ExperimentData` (the load seam); nothing in the Domain depends on it.
module ExperimentDataLoad =

    /// Resolve an experiment's detector kind through the Library (spec 034/037): the captured setup's
    /// detector descriptor binds a protected preset by its entry id (`det-intensity` / `det-ellipsometer`),
    /// which resolves through the read-only `LibraryProxy` to the `DetectorKind` that fixes the expected
    /// data-file shape. Defaults to `Intensity` when the setup captured no detector, its binding is not a
    /// preset id, or the id does not resolve — mirroring the live scene's `runDetectorKind` default.
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

    /// Load + validate an experiment's measured-data file through the proxies (spec 0038 Part L, step
    /// 037): read the file at `path` through the `ExperimentDataProxy` in the shape the experiment's
    /// detector kind fixes (`tryLoadIntensity` for an intensity detector, `tryLoadEllipsometric` for an
    /// ellipsometer), then validate the parsed series against the experiment's varied parameter and
    /// numeric range through `MeasuredData.validateAgainstExperiment`. Returns the validated
    /// `MeasuredSeries` on success, or the typed `ExperimentDataError` a parse, empty-file, range or
    /// units mismatch raises. Pure given the proxies — the disk read is behind the proxy.
    let loadAndValidate
        (library : LibraryProxy)
        (dataProxy : ExperimentDataProxy)
        (experiment : Experiment)
        (path : DataFilePath)
        : Result<MeasuredSeries, ExperimentDataError> =
        let detector = detectorKindOf library experiment
        let varied = experiment.varied |> Option.map (fun v -> v.variable)
        match detector with
        | Intensity ->
            dataProxy.tryLoadIntensity path
            |> Result.map IntensityData
            |> Result.bind (validateAgainstExperiment varied experiment.range detector)
        | Ellipsometer ->
            dataProxy.tryLoadEllipsometric path
            |> Result.map EllipsometricData
            |> Result.bind (validateAgainstExperiment varied experiment.range detector)
