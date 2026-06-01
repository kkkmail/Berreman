namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Domain.Units
open Xunit

/// Units-spine boundary conversions — AC-A5 (SI storage), AC-A7 (eV boundary
/// round-trip), AC-D1 (eV via 1239.84), AC-D2 (length round-trips against the
/// `Constants.fs` factors). Proves conversions reduce to the engine canonical
/// base and never persist a non-SI unit.
module UnitsTests =

    let private relClose (expected : float) (actual : float) (relTol : float) =
        let denom = if abs expected < 1e-300 then 1.0 else abs expected
        abs (actual - expected) / denom <= relTol

    [<Fact>]
    let ``AC-D1 toWaveLength ElectronVolt 13.5 reduces via 1239.84 within 1e-15 m`` () =
        let w = toWaveLength ElectronVolt 13.5
        // E[eV] = 1239.84 / lambda[nm]; reduce nm -> meter through nmToMeter.
        let expected = (1239.84 / 13.5) * 1.0e-9
        let actual = w.value / 1.0<meter>
        Assert.True(abs (actual - expected) <= 1e-15, $"expected {expected} m, got {actual} m")

    [<Fact>]
    let ``AC-A7 eV boundary round-trips through wavelength-in-meters`` () =
        let ev = 13.5
        let w = toWaveLength ElectronVolt ev
        let backEv = wavelengthToUnit ElectronVolt w
        Assert.True(relClose ev backEv 1e-12, $"eV round-trip: {backEv}")
        // The stored quantity is meters (SI), positive — eV is never persisted.
        Assert.True(w.value / 1.0<meter> > 0.0)

    [<Fact>]
    let ``AC-A5 boundary stores quantities in SI meters`` () =
        // 600 nm entered at the boundary is stored as 6.0e-7 m in the canonical base.
        let m = (toMeters Nanometer 600.0) / 1.0<meter>
        Assert.True(relClose 6.0e-7 m 1e-12, $"got {m} m")

    [<Fact>]
    let ``AC-D2 length units round-trip toMeters/fromMeters within 1e-12`` () =
        let units = [ Meter; Millimeter; Micrometer; Nanometer; Angstrom ]
        let value = 123.456
        for u in units do
            let m = toMeters u value
            let back = fromMeters u m
            Assert.True(relClose value back 1e-12, $"unit {u} round-trip failed: {back}")

    [<Fact>]
    let ``Wavenumber boundary maps via lambda_nm = 1e7 / nu and round-trips`` () =
        let nu = 5000.0 // cm^-1 -> 2000 nm
        let w = toWaveLength Wavenumber nu
        let actualNm = (w.value / nmToMeter) / 1.0<nm>
        Assert.True(relClose (1.0e7 / nu) actualNm 1e-12, $"nm: {actualNm}")
        let back = wavelengthToUnit Wavenumber w
        Assert.True(relClose nu back 1e-12, $"cm^-1 round-trip: {back}")

    [<Fact>]
    let ``R-8 toWaveLength returns native engine Nm/Mkm cases, never a new case`` () =
        match toWaveLength Nanometer 600.0 with
        | Nm _ -> ()
        | other -> Assert.Fail($"expected Nm, got {other}")

        match toWaveLength Micrometer 0.6 with
        | Mkm _ -> ()
        | other -> Assert.Fail($"expected Mkm, got {other}")

        // Angstrom cannot be represented natively -> reduces to nm and returns Nm.
        match toWaveLength Angstrom 6000.0 with
        | Nm _ -> ()
        | other -> Assert.Fail($"expected Nm for Angstrom, got {other}")
