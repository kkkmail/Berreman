namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Ui.MaterialPreview
open Xunit

/// Spectral-preview routing + unit toggle (§D.10, AC-D7). Proves the spectral-axis
/// display unit never alters the canonical-meter `Range<WaveLength>` handed to the
/// engine plots — only the axis labels/ticks rescale.
module MaterialPreviewTests =

    let private entry = builtInEntries |> List.head

    [<Fact>]
    let ``AC-D7 the spectral-axis unit does not alter the canonical Range<WaveLength>`` () =
        let range = spectralRange Nanometer 400.0 700.0 50
        let (_, rNm) = plotInput entry range Nanometer
        let (_, rEv) = plotInput entry range ElectronVolt
        Assert.Equal<Range<WaveLength>>(rNm, rEv)
        Assert.Equal<Range<WaveLength>>(range, rNm)

    [<Fact>]
    let ``spectralRange stores canonical meters regardless of the entry unit`` () =
        let viaNm = spectralRange Nanometer 500.0 500.0 1
        Assert.True(viaNm.startValue.value / 1.0<meter> > 0.0)
        // The same physical point entered in eV reduces to the same meters.
        let ev = wavelengthToUnit ElectronVolt (WaveLength.nm 500.0<nm>)
        let viaEv = spectralRange ElectronVolt ev ev 1
        let diff = abs (viaEv.startValue.value / 1.0<meter> - viaNm.startValue.value / 1.0<meter>)
        Assert.True(diff <= 1e-15, $"meter mismatch {diff}")

    [<Fact>]
    let ``axisLabel and axisTicks are display-only (relabel and rescale)`` () =
        let range = spectralRange Nanometer 400.0 700.0 3
        Assert.NotEqual<string>(axisLabel Nanometer, axisLabel ElectronVolt)
        let nmTicks = axisTicks Nanometer range
        let evTicks = axisTicks ElectronVolt range
        Assert.Equal(nmTicks.Length, evTicks.Length)
        Assert.NotEqual<float>(List.head nmTicks, List.head evTicks)
