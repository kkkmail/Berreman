namespace OpticalConstructor.Tests

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open Xunit

/// Spec 0050 S11.3 — a fit run's shape: its name from its study and the clock, its stage, calibration-use and engine
/// tags, and the failure a multi-start member records.
module FitRunTests =

    [<Fact>]
    let ``a fit run is named by its study's file stem and the clock's instant`` () : unit =
        let name = (StudyName "QZ:LR").fitRunNameAt (ClockTime (DateTime(2026, 11, 1, 12, 0, 0, DateTimeKind.Utc)))

        Assert.Equal(FitRunName "QZ_LR@20261101T120000Z", name)
        Assert.Equal(FileStem "QZ_LR@20261101T120000Z", name.fileStem)
        Assert.Equal(FileName "QZ_LR@20261101T120000Z.fitrun-json", FitRunKitFile.fileNameOf name.fileStem)

    [<Fact>]
    let ``a fit stage, a calibration use and an engine round-trip their wire tags and render their text`` () : unit =
        Assert.Equal<FitStage list>([ JointStage; CalibrationStage; ObjectStage ], FitStage.all)
        Assert.Equal<int list>([ 1; 2; 3 ], FitStage.all |> List.map (fun stage -> stage.value))
        Assert.Equal<string list>(
            [ "Joint"; "Stage 1 (calibration)"; "Stage 2 (objects)" ],
            FitStage.all |> List.map (fun stage -> stage.text))

        for stage in FitStage.all do
            Assert.Equal<FitStage option>(Some stage, FitStage.tryOfWire stage.value)

        for tag in [ 0; 4 ] do
            Assert.Equal<FitStage option>(None, FitStage.tryOfWire tag)

        Assert.Equal<CalibrationUse list>([ CalibrationHeldFixed; CalibrationAsPrior ], CalibrationUse.all)
        Assert.Equal<int list>([ 1; 2 ], CalibrationUse.all |> List.map (fun calibrationUse -> calibrationUse.value))
        Assert.Equal<string list>([ "fixed"; "as priors" ], CalibrationUse.all |> List.map (fun calibrationUse -> calibrationUse.text))

        for calibrationUse in CalibrationUse.all do
            Assert.Equal<CalibrationUse option>(Some calibrationUse, CalibrationUse.tryOfWire calibrationUse.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<CalibrationUse option>(None, CalibrationUse.tryOfWire tag)

        Assert.Equal<StudyEngineKind list>([ LinearStudyEngine; NonlinearStudyEngine ], StudyEngineKind.all)
        Assert.Equal<int list>([ 1; 2 ], StudyEngineKind.all |> List.map (fun engine -> engine.value))
        Assert.Equal<string list>([ "linear"; "nonlinear" ], StudyEngineKind.all |> List.map (fun engine -> engine.text))

        for engine in StudyEngineKind.all do
            Assert.Equal<StudyEngineKind option>(Some engine, StudyEngineKind.tryOfWire engine.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<StudyEngineKind option>(None, StudyEngineKind.tryOfWire tag)

    [<Fact>]
    let ``a solver failure renders the reason a multi-start member failed`` () : unit =
        let notConverged = (DidNotConvergeErr "step too small").failureText
        let invalid = (InvalidNonlinearRequestErr "empty").failureText

        Assert.Equal(FitMemberFailure "the solver did not converge: step too small", notConverged)
        Assert.Equal(FitMemberFailure "the solve request is not valid: empty", invalid)
        Assert.EndsWith("step too small", notConverged.value)
        Assert.EndsWith("empty", invalid.value)
