namespace Analytics

open Berreman.Solvers
open Berreman.FieldFunctions
open Analytics.Variables

/// Color conversion of an R or T spectrum (Part F §F.8). `spectrumToXyz` integrates a
/// chosen channel (R or T) — read from the `SweepPoint` array that
/// `Variables.calculate` produces over a `WaveLengthRange` — against the embedded CIE
/// 1931 2° colour-matching functions and a chosen illuminant; `CieXyz.toLab`/`toSrgb`
/// apply the standard CIE Lab and sRGB transforms. The colour-matching and D65/A
/// illuminant tables are the only net-new numeric data this part introduces (traced to
/// 010 §4 "Color calculation"). These functions consume the §F.1 sweep output and NEVER
/// re-run the solver; no colour space beyond XYZ/Lab/sRGB is added.
module Colorimetry =

    /// Choice of standard illuminant for `spectrumToXyz`.
    type Illuminant =
        | D65
        | IlluminantA


    /// A CIE 1931 tristimulus value X, Y or Z, scaled so a perfect reflector under the chosen illuminant has Y = 1.
    type TristimulusValue =
        | TristimulusValue of float

        member this.value = let (TristimulusValue v) = this in v


    /// A CIE L*a*b* coordinate: the lightness L*, or one of the opponent-colour axes a*, b*.
    type LabCoordinate =
        | LabCoordinate of float

        member this.value = let (LabCoordinate v) = this in v


    /// One gamma-encoded sRGB channel, in [0, 1].
    type SrgbChannel =
        | SrgbChannel of float

        member this.value = let (SrgbChannel v) = this in v


    /// A colour in CIE L*a*b* (D65 reference white).
    type CieLab =
        {
            lStar : LabCoordinate
            aStar : LabCoordinate
            bStar : LabCoordinate
        }


    /// An sRGB swatch (D65).
    type SrgbSwatch =
        {
            red : SrgbChannel
            green : SrgbChannel
            blue : SrgbChannel
        }


    /// A colour in CIE 1931 XYZ.
    type CieXyz =
        {
            cieX : TristimulusValue
            cieY : TristimulusValue
            cieZ : TristimulusValue
        }

        /// CIE D65 reference white point used by the Lab transform and assumed by the sRGB
        /// matrix below.
        static member private d65White : CieXyz =
            { cieX = TristimulusValue 0.95047; cieY = TristimulusValue 1.0; cieZ = TristimulusValue 1.08883 }

        /// §F.8 — CIE XYZ → CIE L*a*b* (D65 reference white).
        member this.toLab : CieLab =
            let white = CieXyz.d65White
            let f t =
                if t > 0.008856 then t ** (1.0 / 3.0)
                else 7.787 * t + 16.0 / 116.0
            let fx = f (this.cieX.value / white.cieX.value)
            let fy = f (this.cieY.value / white.cieY.value)
            let fz = f (this.cieZ.value / white.cieZ.value)
            {
                lStar = LabCoordinate (116.0 * fy - 16.0)
                aStar = LabCoordinate (500.0 * (fx - fy))
                bStar = LabCoordinate (200.0 * (fy - fz))
            }

        /// §F.8 — CIE XYZ → sRGB swatch (D65, standard matrix + gamma), clamped to [0, 1].
        member this.toSrgb : SrgbSwatch =
            let x = this.cieX.value
            let y = this.cieY.value
            let z = this.cieZ.value
            let r =  3.2406 * x - 1.5372 * y - 0.4986 * z
            let g = -0.9689 * x + 1.8758 * y + 0.0415 * z
            let b =  0.0557 * x - 0.2040 * y + 1.0570 * z
            let gamma c =
                let c = max 0.0 (min 1.0 c)
                if c <= 0.0031308 then 12.92 * c
                else 1.055 * (c ** (1.0 / 2.4)) - 0.055
            { red = SrgbChannel (gamma r); green = SrgbChannel (gamma g); blue = SrgbChannel (gamma b) }


    /// One row of the CIE 1931 2° colour-matching table: the wavelength in nm and x̄, ȳ, z̄ there.
    [<Softellect.Sys.NumericCarrier("a row of a tabulated spectrum (R3): the CIE 1931 colour-matching functions at one wavelength, interpolated at every sample of the XYZ integral")>]
    type private ColourMatchingSample =
        {
            waveLengthNm : float
            xBar : float
            yBar : float
            zBar : float
        }


    /// One row of the standard-illuminant table: the wavelength in nm and the relative spectral power of D65 and A there.
    [<Softellect.Sys.NumericCarrier("a row of a tabulated spectrum (R3): the D65 and A relative spectral powers at one wavelength, interpolated at every sample of the XYZ integral")>]
    type private IlluminantSample =
        {
            waveLengthNm : float
            d65 : float
            illuminantA : float
        }


    /// One sample of a tabulated curve: the wavelength in nm and the curve's value there.
    [<Softellect.Sys.NumericCarrier("an element of a tabulated spectrum (R3): one (wavelength, value) point the interpolation reads in its per-sample loop")>]
    type private SpectralSample =
        {
            waveLengthNm : float
            sampleValue : float
        }


    /// A tabulated curve on ascending wavelengths.
    type private SpectralCurve =
        | SpectralCurve of SpectralSample[]

        /// Linear interpolation at `wl` nm, clamped at the endpoints.
        member this.interpolate (wl : float) : float =
            let (SpectralCurve table) = this
            let n = table.Length
            let wl0 = table.[0].waveLengthNm
            let v0 = table.[0].sampleValue
            let wlN = table.[n - 1].waveLengthNm
            let vN = table.[n - 1].sampleValue
            if wl <= wl0 then v0
            elif wl >= wlN then vN
            else
                let mutable result = vN
                let mutable i = 0
                let mutable found = false
                while not found && i < n - 1 do
                    let wlA = table.[i].waveLengthNm
                    let vA = table.[i].sampleValue
                    let wlB = table.[i + 1].waveLengthNm
                    let vB = table.[i + 1].sampleValue
                    if wl >= wlA && wl <= wlB then
                        let f = if wlB = wlA then 0.0 else (wl - wlA) / (wlB - wlA)
                        result <- vA + f * (vB - vA)
                        found <- true
                    else
                        i <- i + 1
                result


    /// CIE 1931 2° colour-matching functions at 10 nm.
    let private cie1931 : ColourMatchingSample[] =
        [|
            { waveLengthNm = 380.0; xBar = 0.0014; yBar = 0.0000; zBar = 0.0065 }
            { waveLengthNm = 390.0; xBar = 0.0042; yBar = 0.0001; zBar = 0.0201 }
            { waveLengthNm = 400.0; xBar = 0.0143; yBar = 0.0004; zBar = 0.0679 }
            { waveLengthNm = 410.0; xBar = 0.0435; yBar = 0.0012; zBar = 0.2074 }
            { waveLengthNm = 420.0; xBar = 0.1344; yBar = 0.0040; zBar = 0.6456 }
            { waveLengthNm = 430.0; xBar = 0.2839; yBar = 0.0116; zBar = 1.3856 }
            { waveLengthNm = 440.0; xBar = 0.3483; yBar = 0.0230; zBar = 1.7471 }
            { waveLengthNm = 450.0; xBar = 0.3362; yBar = 0.0380; zBar = 1.7721 }
            { waveLengthNm = 460.0; xBar = 0.2908; yBar = 0.0600; zBar = 1.6692 }
            { waveLengthNm = 470.0; xBar = 0.1954; yBar = 0.0910; zBar = 1.2876 }
            { waveLengthNm = 480.0; xBar = 0.0956; yBar = 0.1390; zBar = 0.8130 }
            { waveLengthNm = 490.0; xBar = 0.0320; yBar = 0.2080; zBar = 0.4652 }
            { waveLengthNm = 500.0; xBar = 0.0049; yBar = 0.3230; zBar = 0.2720 }
            { waveLengthNm = 510.0; xBar = 0.0093; yBar = 0.5030; zBar = 0.1582 }
            { waveLengthNm = 520.0; xBar = 0.0633; yBar = 0.7100; zBar = 0.0782 }
            { waveLengthNm = 530.0; xBar = 0.1655; yBar = 0.8620; zBar = 0.0422 }
            { waveLengthNm = 540.0; xBar = 0.2904; yBar = 0.9540; zBar = 0.0203 }
            { waveLengthNm = 550.0; xBar = 0.4334; yBar = 0.9950; zBar = 0.0087 }
            { waveLengthNm = 560.0; xBar = 0.5945; yBar = 0.9950; zBar = 0.0039 }
            { waveLengthNm = 570.0; xBar = 0.7621; yBar = 0.9520; zBar = 0.0021 }
            { waveLengthNm = 580.0; xBar = 0.9163; yBar = 0.8700; zBar = 0.0017 }
            { waveLengthNm = 590.0; xBar = 1.0263; yBar = 0.7570; zBar = 0.0011 }
            { waveLengthNm = 600.0; xBar = 1.0622; yBar = 0.6310; zBar = 0.0008 }
            { waveLengthNm = 610.0; xBar = 1.0026; yBar = 0.5030; zBar = 0.0003 }
            { waveLengthNm = 620.0; xBar = 0.8544; yBar = 0.3810; zBar = 0.0002 }
            { waveLengthNm = 630.0; xBar = 0.6424; yBar = 0.2650; zBar = 0.0000 }
            { waveLengthNm = 640.0; xBar = 0.4479; yBar = 0.1750; zBar = 0.0000 }
            { waveLengthNm = 650.0; xBar = 0.2835; yBar = 0.1070; zBar = 0.0000 }
            { waveLengthNm = 660.0; xBar = 0.1649; yBar = 0.0610; zBar = 0.0000 }
            { waveLengthNm = 670.0; xBar = 0.0874; yBar = 0.0320; zBar = 0.0000 }
            { waveLengthNm = 680.0; xBar = 0.0468; yBar = 0.0170; zBar = 0.0000 }
            { waveLengthNm = 690.0; xBar = 0.0227; yBar = 0.0082; zBar = 0.0000 }
            { waveLengthNm = 700.0; xBar = 0.0114; yBar = 0.0041; zBar = 0.0000 }
            { waveLengthNm = 710.0; xBar = 0.0058; yBar = 0.0021; zBar = 0.0000 }
            { waveLengthNm = 720.0; xBar = 0.0029; yBar = 0.0010; zBar = 0.0000 }
            { waveLengthNm = 730.0; xBar = 0.0014; yBar = 0.0005; zBar = 0.0000 }
            { waveLengthNm = 740.0; xBar = 0.0007; yBar = 0.0002; zBar = 0.0000 }
            { waveLengthNm = 750.0; xBar = 0.0003; yBar = 0.0001; zBar = 0.0000 }
            { waveLengthNm = 760.0; xBar = 0.0002; yBar = 0.0001; zBar = 0.0000 }
            { waveLengthNm = 770.0; xBar = 0.0001; yBar = 0.0000; zBar = 0.0000 }
            { waveLengthNm = 780.0; xBar = 0.0000; yBar = 0.0000; zBar = 0.0000 }
        |]

    /// Relative spectral power of the standard illuminants at 10 nm.
    let private illuminantTable : IlluminantSample[] =
        [|
            { waveLengthNm = 380.0; d65 =  49.98; illuminantA =   9.80 }
            { waveLengthNm = 390.0; d65 =  54.65; illuminantA =  12.09 }
            { waveLengthNm = 400.0; d65 =  82.75; illuminantA =  14.71 }
            { waveLengthNm = 410.0; d65 =  91.49; illuminantA =  17.68 }
            { waveLengthNm = 420.0; d65 =  93.43; illuminantA =  21.00 }
            { waveLengthNm = 430.0; d65 =  86.68; illuminantA =  24.67 }
            { waveLengthNm = 440.0; d65 = 104.87; illuminantA =  28.70 }
            { waveLengthNm = 450.0; d65 = 117.01; illuminantA =  33.09 }
            { waveLengthNm = 460.0; d65 = 117.81; illuminantA =  37.81 }
            { waveLengthNm = 470.0; d65 = 114.86; illuminantA =  42.87 }
            { waveLengthNm = 480.0; d65 = 115.92; illuminantA =  48.24 }
            { waveLengthNm = 490.0; d65 = 108.81; illuminantA =  53.91 }
            { waveLengthNm = 500.0; d65 = 109.35; illuminantA =  59.86 }
            { waveLengthNm = 510.0; d65 = 107.80; illuminantA =  66.06 }
            { waveLengthNm = 520.0; d65 = 104.79; illuminantA =  72.50 }
            { waveLengthNm = 530.0; d65 = 107.69; illuminantA =  79.13 }
            { waveLengthNm = 540.0; d65 = 104.41; illuminantA =  85.95 }
            { waveLengthNm = 550.0; d65 = 104.05; illuminantA =  92.91 }
            { waveLengthNm = 560.0; d65 = 100.00; illuminantA = 100.00 }
            { waveLengthNm = 570.0; d65 =  96.33; illuminantA = 107.18 }
            { waveLengthNm = 580.0; d65 =  95.79; illuminantA = 114.44 }
            { waveLengthNm = 590.0; d65 =  88.69; illuminantA = 121.73 }
            { waveLengthNm = 600.0; d65 =  90.01; illuminantA = 129.04 }
            { waveLengthNm = 610.0; d65 =  89.60; illuminantA = 136.35 }
            { waveLengthNm = 620.0; d65 =  87.70; illuminantA = 143.62 }
            { waveLengthNm = 630.0; d65 =  83.29; illuminantA = 150.84 }
            { waveLengthNm = 640.0; d65 =  83.70; illuminantA = 157.98 }
            { waveLengthNm = 650.0; d65 =  80.03; illuminantA = 165.03 }
            { waveLengthNm = 660.0; d65 =  80.21; illuminantA = 171.96 }
            { waveLengthNm = 670.0; d65 =  82.28; illuminantA = 178.77 }
            { waveLengthNm = 680.0; d65 =  78.28; illuminantA = 185.43 }
            { waveLengthNm = 690.0; d65 =  69.72; illuminantA = 191.93 }
            { waveLengthNm = 700.0; d65 =  71.61; illuminantA = 198.26 }
            { waveLengthNm = 710.0; d65 =  74.35; illuminantA = 204.41 }
            { waveLengthNm = 720.0; d65 =  61.60; illuminantA = 210.36 }
            { waveLengthNm = 730.0; d65 =  69.89; illuminantA = 216.12 }
            { waveLengthNm = 740.0; d65 =  75.09; illuminantA = 221.67 }
            { waveLengthNm = 750.0; d65 =  63.59; illuminantA = 227.00 }
            { waveLengthNm = 760.0; d65 =  46.42; illuminantA = 232.12 }
            { waveLengthNm = 770.0; d65 =  66.81; illuminantA = 237.01 }
            { waveLengthNm = 780.0; d65 =  63.38; illuminantA = 241.68 }
        |]

    let private xBarCurve = cie1931 |> Array.map (fun r -> { waveLengthNm = r.waveLengthNm; sampleValue = r.xBar }) |> SpectralCurve
    let private yBarCurve = cie1931 |> Array.map (fun r -> { waveLengthNm = r.waveLengthNm; sampleValue = r.yBar }) |> SpectralCurve
    let private zBarCurve = cie1931 |> Array.map (fun r -> { waveLengthNm = r.waveLengthNm; sampleValue = r.zBar }) |> SpectralCurve
    let private d65Curve = illuminantTable |> Array.map (fun r -> { waveLengthNm = r.waveLengthNm; sampleValue = r.d65 }) |> SpectralCurve
    let private illuminantACurve = illuminantTable |> Array.map (fun r -> { waveLengthNm = r.waveLengthNm; sampleValue = r.illuminantA }) |> SpectralCurve

    /// §F.8 — integrate a chosen channel (R or T) over the sweep produced by
    /// `Variables.calculate` (whose `plotValue` is the wavelength in nm) against
    /// the CIE 1931 2° CMFs and the chosen illuminant, returning CIE XYZ. The luminance Y
    /// is normalised so a perfect reflector (channel ≡ 1) yields the illuminant's white
    /// point (Y = 1). Consumes the §F.1 sweep output; does NOT re-run the solver.
    let spectrumToXyz
        (illuminant : Illuminant)
        (channel : OpticalFunction)
        (spectrum : SweepPoint[])
        : CieXyz =

        let illum =
            match illuminant with
            | D65 -> d65Curve
            | IlluminantA -> illuminantACurve

        let samples =
            spectrum
            |> Array.map (fun p -> (p.plotValue, (p.solution.func channel |> Option.defaultValue OpticalFunctionValue.zero).value))

        let norm =
            samples
            |> Array.sumBy (fun (wl, _) -> illum.interpolate wl * yBarCurve.interpolate wl)

        let k = if abs norm < 1.0e-300 then 0.0 else 1.0 / norm

        {
            cieX = TristimulusValue (k * (samples |> Array.sumBy (fun (wl, r) -> r * illum.interpolate wl * xBarCurve.interpolate wl)))
            cieY = TristimulusValue (k * (samples |> Array.sumBy (fun (wl, r) -> r * illum.interpolate wl * yBarCurve.interpolate wl)))
            cieZ = TristimulusValue (k * (samples |> Array.sumBy (fun (wl, r) -> r * illum.interpolate wl * zBarCurve.interpolate wl)))
        }
