namespace OpticalConstructor.Storage

open System.IO
open OpticalConstructor.Domain.Experiments            // DataFilePath (step 025)
open OpticalConstructor.Domain.MeasuredData           // IntensitySeries / EllipsometricSeries / ExperimentDataError + the pure parsers (step 034)
open OpticalConstructor.Domain.ExperimentData         // ExperimentDataProxy — the DECLARED load seam (step 035)

/// Spec 0038 Part I (036, IMPLEMENT_CONTRACT STORE_XDUO_0006) — the real file-backed adapter behind the
/// DECLARED `ExperimentDataProxy` (step 035). This is the ONLY new real IO this spec permits
/// (spec-md §0.3c): the missing `path -> read text -> parse` EDGE the pure step-34 `MeasuredData`
/// parsers were written to sit behind. `MeasuredData.parseIntensitySeries` / `parseEllipsometricSeries`
/// take the file TEXT and never touch the filesystem or throw, so this adapter is exactly the disk read
/// that yields that text — and NOTHING else. It adds NO parsing logic of its own: the schema, the
/// malformed-row / empty-file diagnostics, and the elevated point types all stay in `MeasuredData`.
///
/// Filesystem access is the boundary. `File.ReadAllText` is wrapped so any .NET IO exception (a missing
/// file, a directory that does not exist, an access failure) is caught HERE and mapped to a typed
/// `ExperimentDataError` carrying the failure reason — an exception NEVER crosses into the pure Domain.
/// This mirrors the storage-layer precedent `ProjectFile.openProject`
/// (`try Ok (File.ReadAllText path) with e -> Error (FileIoError e)`).
///
/// `createFileBacked` is the runtime replacement for the step-035 in-memory mock (canned series keyed by
/// path, in `OpticalConstructor.Tests`): every consumer that holds an `ExperimentDataProxy` is unchanged
/// — only the composition root swaps the mock for `createFileBacked ()`.
module ExperimentDataStore =

    /// Read the file at `path` and hand its TEXT to `parse` (a step-34 pure parser). Filesystem access is
    /// the boundary: a missing file, a missing directory, or any other .NET IO exception is caught HERE
    /// and mapped to a typed `MalformedDataFile` naming the file kind, the path, and the underlying reason
    /// — never a throw across into the pure Domain, and staying WITHIN the step-34 declared four-case
    /// channel (the step-035 mock returned the same case for a missing file). The parse itself is delegated
    /// UNCHANGED, so every schema / malformed-row / empty-file diagnostic is the step-34 parser's, not this
    /// adapter's.
    let private loadThrough
        (parse : string -> Result<'Series, ExperimentDataError>)
        (kind : string)
        (path : DataFilePath)
        : Result<'Series, ExperimentDataError> =
        let read =
            try Ok (File.ReadAllText path.value)
            with e -> Error (MalformedDataFile $"could not read the {kind} data file '{path.value}': {e.Message}")
        match read with
        | Error e -> Error e
        | Ok text -> parse text

    /// Build the real file-backed `ExperimentDataProxy` (spec 036): both fields resolve an experiment's
    /// elevated `DataFilePath` by reading the file's text at this IO boundary and delegating to the step-34
    /// `MeasuredData` parser — `parseIntensitySeries` for an intensity `X,Y` file, `parseEllipsometricSeries`
    /// for a `wavelength_nm,psi_deg,delta_deg[,aoi_deg]` file. Replaces the step-035 in-memory mock for a
    /// real store, leaving every consumer that holds the proxy unchanged.
    let createFileBacked () : ExperimentDataProxy =
        {
            tryLoadIntensity = loadThrough parseIntensitySeries "intensity"
            tryLoadEllipsometric = loadThrough parseEllipsometricSeries "ellipsometric"
        }
