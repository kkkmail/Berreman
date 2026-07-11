namespace OpticalConstructor.Ui.Tests

open Xunit
open Berreman.Fields
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Controls.ExperimentChart
open OpticalConstructor.Controls.ChartStyle
open OpticalConstructor.Ui

/// Spec 0038 (032) — the Material editor's tabbed multi-curve preview builders on the 018 dual-axis
/// spine. Headless MODEL tests (no window): `nkDispersionChart` yields PER PRINCIPAL AXIS n curves on
/// the LEFT axis and k curves on the RIGHT (six series through the SAME builder — an isotropic entry
/// draws coincident curves, a biaxial one three distinct pairs); `gyrationChart` yields the gyration
/// tensor components for an active entry; `muChart` yields the Polder μ components for a magnetic one.
/// A dispersive entry yields curves and a non-dispersive one flat lines through ONE `getEps` path; the
/// display unit rescales only the x-axis (AC-D7).
module NkDispersionChartTests =

    /// Silicon — a DISPERSIVE built-in (engine preset, dispersion coded, `complexity = None`).
    let private siliconEntry : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.silicon)

    /// Vacuum — a NON-dispersive built-in (n = 1, k = 0 at every wavelength).
    let private vacuumEntry : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.vacuum)

    let private applyOk (msg : MaterialComplexityMsg) (s : MaterialComplexityEditState) : MaterialComplexityEditState =
        match applyMaterialComplexityMsg msg s with
        | Ok next -> next
        | Error e -> failwith $"unexpected edit rejection: %A{e}"

    /// The engine properties derived from a fold of ladder edits over the default edit state.
    let private propsOf (msgs : MaterialComplexityMsg list) : OpticalPropertiesWithDisp =
        let st = msgs |> List.fold (fun s m -> applyOk m s) MaterialComplexityEditor.defaultState
        match toComplexity st with
        | Ok c -> c.toProperties
        | Error e -> failwith $"expected a derivable complexity, got %A{e}"

    let private seriesNamed (chart : ExperimentChart) (name : string) : ChartSeries =
        chart.series |> List.find (fun s -> s.name = name)

    let private firstY (chart : ExperimentChart) (name : string) : float =
        (seriesNamed chart name).points |> List.head |> snd

    // ============================ the per-axis n/k builder ============================

    [<Fact>]
    let ``AC: per-axis n lands on the left axis, k on the right, spanning the requested range`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        // Six series in a fixed order: n₁ n₂ n₃ then k₁ k₂ k₃, sharing one x grid.
        Assert.Equal<string list>([ "n₁"; "n₂"; "n₃"; "k₁"; "k₂"; "k₃" ], chart.series |> List.map (fun s -> s.name))
        let xs = (List.head chart.series).points |> List.map fst
        for s in chart.series do
            Assert.Equal<float list>(xs, s.points |> List.map fst)
        // The x-axis spans the REQUESTED range in the display unit: 400…800 nm over 41 grid points.
        Assert.Equal(41, List.length xs)
        Assert.True(abs (List.head xs - 400.0) < 1e-9, $"first x = %g{List.head xs}")
        Assert.True(abs (List.last xs - 800.0) < 1e-9, $"last x = %g{List.last xs}")
        Assert.Equal<string>(SpectralAxis.axisLabel Nanometer, chart.xLabel)
        Assert.False(chart.angular)
        // The paired style seed carries the axis assignment (the side is STYLE, not data — 018):
        // n₁/n₂/n₃ stay on the LEFT axis, k₁/k₂/k₃ are flipped to the RIGHT.
        let style = NkDispersionChart.nkDispersionStyle chart
        for i in 0 .. 2 do
            Assert.Equal(LeftAxis, (seriesStyleOf i style).axisSide)
        for i in 3 .. 5 do
            Assert.Equal(RightAxis, (seriesStyleOf i style).axisSide)

    [<Fact>]
    let ``a dispersive entry yields a curve — silicon's n₁ varies across 400…800 nm`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        let ys = (seriesNamed chart "n₁").points |> List.map snd
        let span = List.max ys - List.min ys
        Assert.True(span > 0.01, $"silicon n₁ span across the range = %g{span} (expected a curve)")

    [<Fact>]
    let ``a non-dispersive entry yields flat lines through the SAME builder — vacuum n = 1, k = 0 on every axis`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart vacuumEntry.properties Nanometer range
        for s in chart.series do
            let ys = s.points |> List.map snd
            Assert.True(List.max ys - List.min ys < 1e-12, $"series %s{s.name} is not flat")
        for name in [ "n₁"; "n₂"; "n₃" ] do
            Assert.True(abs (firstY chart name - 1.0) < 1e-12, $"vacuum %s{name} must be 1")
        for name in [ "k₁"; "k₂"; "k₃" ] do
            Assert.True(abs (firstY chart name) < 1e-12, $"vacuum %s{name} must be 0")

    [<Fact>]
    let ``the per-axis builder draws three DISTINCT axes for a biaxial entry`` () =
        let biaxial =
            propsOf
                [
                    ChooseAnisotropy Biaxial
                    SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (createComplex 1.6 0.0))
                    SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (createComplex 1.7 0.0))
                    SetPrincipalIndex (ThirdAxis, ComplexRefractionIndex (createComplex 1.8 0.0))
                ]
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 8
        let chart = NkDispersionChart.nkDispersionChart biaxial Nanometer range
        let n1, n2, n3 = firstY chart "n₁", firstY chart "n₂", firstY chart "n₃"
        Assert.True(abs (n1 - 1.6) < 1e-9, $"n₁ = %g{n1}")
        Assert.True(abs (n2 - 1.7) < 1e-9, $"n₂ = %g{n2}")
        Assert.True(abs (n3 - 1.8) < 1e-9, $"n₃ = %g{n3}")

    [<Fact>]
    let ``the display unit rescales only the x-axis (AC-D7) — the y data is identical in eV and nm`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 8
        let inNm = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        let inEv = NkDispersionChart.nkDispersionChart siliconEntry.properties ElectronVolt range
        for name in [ "n₁"; "n₂"; "n₃"; "k₁"; "k₂"; "k₃" ] do
            Assert.Equal<float list>((seriesNamed inNm name).points |> List.map snd, (seriesNamed inEv name).points |> List.map snd)
        Assert.Equal<string>(SpectralAxis.axisLabel ElectronVolt, inEv.xLabel)
        // eV ticks descend as the wavelength ascends — a rescale of the SAME grid, not a re-sample.
        let xsEv = (List.head inEv.series).points |> List.map fst
        Assert.True(List.head xsEv > List.last xsEv, "eV axis must descend across an ascending-λ grid")

    // ============================ the gyration builder ============================

    [<Fact>]
    let ``the gyration builder emits the six tensor components for an active entry`` () =
        let active = propsOf [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 8
        let chart = NkDispersionChart.gyrationChart active Nanometer range
        Assert.Equal<string list>([ "g₁₁"; "g₂₂"; "g₃₃"; "g₁₂"; "g₁₃"; "g₂₃" ], chart.series |> List.map (fun s -> s.name))
        // A uniaxial active medium carries a non-zero g₁₁ and g₃₃ (the default 1.5e-6 components).
        let peak (name : string) : float = (seriesNamed chart name).points |> List.map (snd >> abs) |> List.max
        Assert.True(peak "g₁₁" > 0.0, "g₁₁ must be non-zero for an active uniaxial medium")
        Assert.True(peak "g₃₃" > 0.0, "g₃₃ must be non-zero for an active uniaxial medium")
        // A view-only classifier agrees the entry is optically active.
        Assert.True(NkDispersionChart.hasGyration active, "an active entry must classify as gyrotropic")

    // ============================ the Polder-μ builder ============================

    [<Fact>]
    let ``the mu builder emits the Polder diagonal and gyration for a magnetic entry`` () =
        let magnetic =
            propsOf
                [
                    SetMagnetic MagneticOn
                    SetMuKind GyromagneticMuKind
                    SetMuDiagonal (MuValue 1.2)
                    SetMuParallel (MuValue 1.1)
                    SetMuGyration (MuValue 0.2)
                    ChooseGyrationAxis AlongZ
                ]
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 8
        let chart = NkDispersionChart.muChart magnetic Nanometer range
        Assert.Equal<string list>([ "μ₁₁"; "μ₂₂"; "μ₃₃"; "g" ], chart.series |> List.map (fun s -> s.name))
        // AlongZ (Faraday): μ[0,0] = μ[1,1] = diagonal 1.2, μ[2,2] = parallel 1.1, gyration magnitude 0.2.
        let mu11, mu33, g = firstY chart "μ₁₁", firstY chart "μ₃₃", firstY chart "g"
        Assert.True(abs (mu11 - 1.2) < 1e-9, $"μ₁₁ = %g{mu11}")
        Assert.True(abs (mu33 - 1.1) < 1e-9, $"μ₃₃ = %g{mu33}")
        Assert.True(abs (g - 0.2) < 1e-9, $"g = %g{g}")
        // The gyration magnitude is flipped to the RIGHT axis; the diagonals stay on the LEFT.
        let style = NkDispersionChart.muStyle chart
        Assert.Equal(RightAxis, (seriesStyleOf 3 style).axisSide)
        Assert.True(NkDispersionChart.hasMagnetic magnetic, "a Polder-μ entry must classify as magnetic")
