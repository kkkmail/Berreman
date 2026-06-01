namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.SourceSpec
open OpticalConstructor.Ui.Sources
open Xunit

/// Part E source-editor projection tests (slice 007). Exercise the Avalonia-free
/// [Core] editor seam (P3): the `SourceSpec.toIncidentLight` projection and the
/// boundary wavelength conversion (AC-E1/AC-E2), the single-value-or-range axis
/// (AC-E3), the polarization presets + live Stokes readout (AC-E4), and the
/// one-click Source palette entry (AC-E10).
module SourceProjectionTests =

    let private tol = 1.0e-12

    let private baseSource = SourceEditorView.defaultSource "src-1"

    let private stokesComponents (s : StokesVector) =
        let (StokesVector v) = s
        (v.[0], v.[1], v.[2], v.[3])

    // ---------------------------------------------------------------- AC-E1

    [<Fact>]
    let ``AC-E1 toIncidentLight returns the entered wavelength/angle/polarization/ellipticity`` () =
        let src =
            baseSource
            |> SourceEditorView.update (SourceEditorView.SetWaveLength (Nanometer, 532.0))
            |> SourceEditorView.update (SourceEditorView.SetIncidenceAngleDegrees 30.0)
            |> SourceEditorView.update (SourceEditorView.SetPolarization Polarization.p)
            |> SourceEditorView.update (SourceEditorView.SetEllipticity 0.25)
        let info = SourceSpec.toIncidentLight src
        // wavelength reduces to canonical meters (532 nm = 5.32e-7 m).
        Assert.Equal(5.32e-7, info.waveLength.value / 1.0<meter>, 15)
        // angle / polarization / ellipticity match the inputs.
        Assert.Equal((Angle.degree 30.0).value, info.incidenceAngle.value, 12)
        Assert.Equal(Polarization.p.value, info.polarization.value, 12)
        Assert.Equal(0.25, info.ellipticity.value, 12)

    [<Fact>]
    let ``AC-E1 a normal-incidence source builds via IncidentLightInfo.create`` () =
        let info = SourceSpec.toIncidentLight baseSource
        // The default source is normal incidence: the engine `create` path.
        Assert.Equal(IncidenceAngle.normal.value, info.incidenceAngle.value, 12)
        Assert.Equal(RefractionIndex.vacuum.value, info.refractionIndex.value, 12)

    // ---------------------------------------------------------------- AC-E2

    [<Theory>]
    [<InlineData("Angstrom", 5320.0)>]
    [<InlineData("Millimeter", 0.000532)>]
    [<InlineData("ElectronVolt", 2.331)>]
    [<InlineData("Wavenumber", 18796.0)>]
    let ``AC-E2 a non-nm/um display unit converts via the boundary and stores no non-SI unit`` (unitName : string) (entered : float) =
        let u =
            match unitName with
            | "Angstrom" -> Angstrom
            | "Millimeter" -> Millimeter
            | "ElectronVolt" -> ElectronVolt
            | "Wavenumber" -> Wavenumber
            | other -> failwith $"unexpected unit {other}"
        let src = baseSource |> SourceEditorView.update (SourceEditorView.SetWaveLength (u, entered))
        // The produced WaveLength carries the §A.10-converted value ...
        Assert.Equal((toMeters u entered) / 1.0<meter>, src.light.waveLength.value / 1.0<meter>, 15)
        // ... and is the engine `Nm` case (the engine has no Å/mm/eV/cm⁻¹ case);
        // the domain model stores no non-SI unit, only the display label.
        match src.light.waveLength with
        | WaveLength.Nm _ -> ()
        | WaveLength.Mkm _ -> Assert.Fail("Å/mm/eV/cm⁻¹ must reduce to the Nm case, not Mkm")
        Assert.Equal(u, src.displayUnit)

    [<Fact>]
    let ``AC-E2 nm and um map directly to the native WaveLength cases`` () =
        let nmSrc = baseSource |> SourceEditorView.update (SourceEditorView.SetWaveLength (Nanometer, 600.0))
        match nmSrc.light.waveLength with
        | WaveLength.Nm v -> Assert.Equal(600.0, v / 1.0<nm>, 9)
        | WaveLength.Mkm _ -> Assert.Fail("nm must map to WaveLength.Nm")
        let umSrc = baseSource |> SourceEditorView.update (SourceEditorView.SetWaveLength (Micrometer, 0.6))
        match umSrc.light.waveLength with
        | WaveLength.Mkm v -> Assert.Equal(0.6, v / 1.0<mkm>, 9)
        | WaveLength.Nm _ -> Assert.Fail("µm must map to WaveLength.Mkm")

    // ---------------------------------------------------------------- AC-E3

    [<Fact>]
    let ``AC-E3 a Ranged wavelength axis yields a WaveLengthRange handed unchanged`` () =
        let axis = SourceEditorView.wavelengthAxisRanged 50
        match SourceEditorView.axisRangedVariable axis with
        | Some (WaveLengthRange _) -> ()
        | other -> Assert.Fail($"expected a WaveLengthRange, got {other}")

    [<Fact>]
    let ``AC-E3 a Ranged angle axis yields an IncidenceAngleRange handed unchanged`` () =
        let axis = SourceEditorView.incidenceAngleAxisRanged 40
        match SourceEditorView.axisRangedVariable axis with
        | Some (IncidenceAngleRange _) -> ()
        | other -> Assert.Fail($"expected an IncidenceAngleRange, got {other}")

    [<Fact>]
    let ``AC-E3 a Fixed axis produces no RangedVariable`` () =
        let axis = SourceEditorView.wavelengthAxisFixed (WaveLength.nm 633.0<nm>)
        Assert.Equal(None, SourceEditorView.axisRangedVariable axis)

    // ---------------------------------------------------------------- AC-E4

    [<Fact>]
    let ``AC-E4 RCP and LCP presets set ellipticity +/-1 via Ellipticity.create`` () =
        let rcp = PolarizationPicker.applyPreset PolarizationPicker.PresetRCP baseSource
        let lcp = PolarizationPicker.applyPreset PolarizationPicker.PresetLCP baseSource
        Assert.Equal(1.0, rcp.light.ellipticity.value, 12)
        Assert.Equal(-1.0, lcp.light.ellipticity.value, 12)

    [<Fact>]
    let ``AC-E4 the unpolarized preset sets Coherence.Unpolarized not a sentinel polarization`` () =
        let unpol = PolarizationPicker.applyPreset PolarizationPicker.PresetUnpolarized baseSource
        Assert.Equal(Unpolarized, unpol.coherence)
        // The polarization/ellipticity are NOT mutated into a sentinel value.
        Assert.Equal(baseSource.light.polarization.value, unpol.light.polarization.value, 12)
        Assert.Equal(baseSource.light.ellipticity.value, unpol.light.ellipticity.value, 12)

    [<Fact>]
    let ``AC-E4 the live Stokes readout is computed via EmField.stokesVector`` () =
        // A circular (RCP) source: |S3| ~ S0 (the engine-computed Stokes vector).
        let rcp = PolarizationPicker.applyPreset PolarizationPicker.PresetRCP baseSource
        let (s0, _, _, s3) = stokesComponents (PolarizationPicker.liveStokes rcp)
        Assert.True(s0 > tol, $"expected a positive intensity S0, got {s0}")
        Assert.Equal(1.0, abs s3 / s0, 6)
        // A linear s source: S3 ~ 0.
        let sLin = PolarizationPicker.applyPreset PolarizationPicker.PresetS baseSource
        let (_, _, _, s3lin) = stokesComponents (PolarizationPicker.liveStokes sLin)
        Assert.True(abs s3lin < 1.0e-6, $"expected ~0 circular component for linear light, got {s3lin}")

    [<Fact>]
    let ``AC-E4 the Poincare marker lies on the unit sphere`` () =
        let rcp = PolarizationPicker.applyPreset PolarizationPicker.PresetRCP baseSource
        let (m1, m2, m3) = PolarizationPicker.poincareMarker rcp
        Assert.Equal(1.0, sqrt (m1 * m1 + m2 * m2 + m3 * m3), 9)
        // RCP sits at the south/north pole: |S3| = 1.
        Assert.Equal(1.0, abs m3, 9)

    // ---------------------------------------------------------------- AC-E10

    [<Fact>]
    let ``AC-E10 the Source palette entry adds a Source element one-click with no gate`` () =
        Assert.Equal(ConstructorElement.Source, SourceEditorView.sourcePaletteEntry.element)
        Assert.False(SourceEditorView.sourcePaletteEntry.requiresConfirmation)
        Assert.False(System.String.IsNullOrWhiteSpace SourceEditorView.sourcePaletteEntry.title)
