namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Ui.Validation
open Xunit

/// §J.9 boundary validators (AC-J9). Non-positive finite thickness, repeat count
/// `< 1`, and an empty/invalid wavelength range each return `Result.Error` WITHOUT
/// throwing or coercing; valid inputs return `Ok` with the value unchanged. Plus
/// the named physical-sanity gain warning (010 Part II §8).
module ValidationTests =

    let private isError (r : Result<'a, ValidationError list>) =
        match r with
        | Error es -> not (List.isEmpty es)
        | Ok _ -> false

    // --- AC-J9: thickness > 0 for finite films (Media.fs:12) --------------------

    [<Fact>]
    let ``AC-J9 zero thickness is rejected`` () =
        Assert.True(isError (Thickness.Thickness 0.0<meter>).validateThickness)

    [<Fact>]
    let ``AC-J9 negative thickness is rejected`` () =
        Assert.True(isError (Thickness.Thickness -1.0e-7<meter>).validateThickness)

    [<Fact>]
    let ``AC-J9 a positive finite thickness is Ok and unchanged`` () =
        let t = Thickness.Thickness 1.0e-7<meter>
        match t.validateThickness with
        | Ok back -> Assert.Equal(t, back)            // no coercion
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``AC-J9 the semi-infinite half-space (Infinity) is Ok`` () =
        match Thickness.Infinity.validateThickness with
        | Ok Thickness.Infinity -> ()
        | other -> Assert.Fail($"%A{other}")

    // --- AC-J9: repeat count >= 1 (the J.2 seam) --------------------------------

    [<Fact>]
    let ``AC-J9 a repeat count below 1 is rejected`` () =
        Assert.True(isError (PeriodCount 0).validateRepeatCount)
        Assert.True(isError (PeriodCount -3).validateRepeatCount)

    [<Fact>]
    let ``AC-J9 a repeat count of 1 or more is Ok and unchanged`` () =
        Assert.Equal<Result<PeriodCount, ValidationError list>>(Ok (PeriodCount 1), (PeriodCount 1).validateRepeatCount)
        Assert.Equal<Result<PeriodCount, ValidationError list>>(Ok (PeriodCount 8), (PeriodCount 8).validateRepeatCount)

    // --- AC-J9: non-empty wavelength range with positive endpoints --------------

    [<Fact>]
    let ``AC-J9 an empty wavelength range (min >= max) is rejected`` () =
        Assert.True(isError (validateWavelengthRange 800.0e-9<meter> 400.0e-9<meter>))
        Assert.True(isError (validateWavelengthRange 500.0e-9<meter> 500.0e-9<meter>))

    [<Fact>]
    let ``AC-J9 a non-positive endpoint is rejected`` () =
        Assert.True(isError (validateWavelengthRange 0.0<meter> 800.0e-9<meter>))
        Assert.True(isError (validateWavelengthRange -1.0e-9<meter> 800.0e-9<meter>))

    [<Fact>]
    let ``AC-J9 a non-finite endpoint is rejected, not thrown`` () =
        // NaN / +inf endpoints must surface as Error, never an exception.
        Assert.True(isError (validateWavelengthRange (System.Double.NaN * 1.0<meter>) 800.0e-9<meter>))
        Assert.True(isError (validateWavelengthRange 400.0e-9<meter> (System.Double.PositiveInfinity * 1.0<meter>)))

    [<Fact>]
    let ``AC-J9 a valid wavelength range is Ok and unchanged`` () =
        match validateWavelengthRange 400.0e-9<meter> 800.0e-9<meter> with
        | Ok range ->
            Assert.Equal(400.0e-9<meter>, range.min)  // no coercion / reordering
            Assert.Equal(800.0e-9<meter>, range.max)
        | Error e -> Assert.Fail($"%A{e}")

    // --- AC-J9: physical-sanity gain warning (advisory, non-blocking) -----------

    [<Fact>]
    let ``AC-J9 a negative imaginary index raises a non-blocking gain warning`` () =
        match (ExtinctionCoefficient -0.01).imaginaryIndexGainWarning with
        | [ w ] -> Assert.Equal(Warning, w.severity)
        | other -> Assert.Fail($"%A{other}")

    [<Fact>]
    let ``AC-J9 an absorbing (k >= 0) index raises no warning`` () =
        Assert.Empty((ExtinctionCoefficient 0.0).imaginaryIndexGainWarning)
        Assert.Empty((ExtinctionCoefficient 0.5).imaginaryIndexGainWarning)
