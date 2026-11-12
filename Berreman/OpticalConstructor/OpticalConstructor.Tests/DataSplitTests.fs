namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.DataSplits

/// One lab file split into groups by its key column, and the key column each import profile splits by (spec 0050 S9.3;
/// 001 §K).
module DataSplitTests =

    let private groupOf (key : DataRowKey) (points : MeasuredPointCount) : KeyedGroup = { key = key; points = points }

    let private assertSplit (expected : KeyedGroup list) (points : MappedPoints) : unit =
        let actual = points.splitGroups

        if actual <> Ok expected then
            Assert.Fail($"expected the groups %A{expected}, got %A{actual}")

    [<Fact>]
    let ``a file splits into one group per key in first-appearance order, each counting its points`` () : unit =
        let a = DataRowKey "A"
        let b = DataRowKey "B"
        let c = DataRowKey "C"
        let zCut = DataRowKey "Z-cut"
        let xCut = DataRowKey "X-cut"
        let k1 = DataRowKey "K1"
        let aLabel = MuellerExperimentLabel "A"
        let bLabel = MuellerExperimentLabel "B"

        let captures =
            CapturePoints (ImportProfileTests.capturesLabelled [ bLabel; aLabel; bLabel; MuellerExperimentLabel "C"; aLabel; bLabel ])

        let observations =
            ObservationPoints [ ImportProfileTests.observationOf zCut; ImportProfileTests.observationOf zCut; ImportProfileTests.observationOf xCut ]

        let labRows =
            ColumnPoints
                [
                    ImportProfileTests.labScanRow (Some k1) (Units.UnitMagnitude 0.0)
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 10.0)
                    ImportProfileTests.labScanRow (Some k1) (Units.UnitMagnitude 20.0)
                ]

        assertSplit [ groupOf b (MeasuredPointCount 3); groupOf a (MeasuredPointCount 2); groupOf c (MeasuredPointCount 1) ] captures
        assertSplit [ groupOf zCut (MeasuredPointCount 2); groupOf xCut (MeasuredPointCount 1) ] observations
        assertSplit [ groupOf k1 (MeasuredPointCount 2) ] labRows
        assertSplit [] (CapturePoints [])

        // A group's points are exactly those the selection of its key keeps.
        for points in [ captures; observations; labRows ] do
            match points.splitGroups with
            | Ok groups ->
                for group in groups do
                    match points.selectedBy (KeyedDataRows group.key) with
                    | Ok selected -> Assert.Equal(group.points, selected.pointCount)
                    | Error error -> Assert.Fail($"the selection of '{group.key.value}' was refused: %A{error}")
            | Error error -> Assert.Fail($"the file was not split: %A{error}")

    [<Fact>]
    let ``a file without a key column does not split, and a profile names the key column it splits by`` () : unit =
        match (SeriesPoints (IntensityData ImportProfileTests.twoPointIntensity)).splitGroups with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("no key column to split by", reason)
        | other -> Assert.Fail($"an X,Y series: expected MalformedDataFileErr, got %A{other}")

        let unkeyed =
            ColumnPoints
                [
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 0.0)
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 20.0)
                ]

        match unkeyed.splitGroups with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("maps no key column", reason)
        | other -> Assert.Fail($"`Lab scan` rows without a key: expected MalformedDataFileErr, got %A{other}")

        let labScan = KitTests.labScanProfile

        let keyless =
            {
                labScan with
                    name = ImportProfileName "Lab scan, no key"
                    roles = { labScan.roles with keyOpt = None }
            }

        let splitKeyOf (name : ImportProfileName) : SplitKeyColumn option =
            match ChosenImportProfile.tryOfName [ labScan; keyless ] name with
            | Some profile -> profile.splitKeyOpt
            | None ->
                Assert.Fail($"no import profile is named '{name.value}'")
                None

        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "experiment"), splitKeyOf ImportProfileName.phoneRoiCsv)
        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "sample"), splitKeyOf ImportProfileName.muellerObservations)
        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "column 4"), splitKeyOf labScan.name)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf ImportProfileName.intensitySeries)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf ImportProfileName.ellipsometricSeries)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf keyless.name)
