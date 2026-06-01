namespace Analytics

open Berreman.Solvers
open Berreman.FieldFunctions

/// Color conversion of an R or T spectrum (Part F §F.8). `spectrumToXyz` integrates a
/// chosen channel (R or T) — read from the `(float * Solution) array` that
/// `Variables.calculate` produces over a `WaveLengthRange` — against the embedded CIE
/// 1931 2° colour-matching functions and a chosen illuminant; `xyzToLab`/`xyzToSrgb`
/// apply the standard CIE Lab and sRGB transforms. The colour-matching and D65/A
/// illuminant tables are the only net-new numeric data this part introduces (traced to
/// 010 §4 "Color calculation"). These functions consume the §F.1 sweep output and NEVER
/// re-run the solver; no colour space beyond XYZ/Lab/sRGB is added.
module Colorimetry =

    /// Choice of standard illuminant for `spectrumToXyz`.
    type Illuminant =
        | D65
        | IlluminantA

    /// CIE 1931 2° colour-matching functions at 10 nm: (λ nm, x̄, ȳ, z̄).
    let private cie1931 : (float * float * float * float)[] =
        [|
            (380.0, 0.0014, 0.0000, 0.0065)
            (390.0, 0.0042, 0.0001, 0.0201)
            (400.0, 0.0143, 0.0004, 0.0679)
            (410.0, 0.0435, 0.0012, 0.2074)
            (420.0, 0.1344, 0.0040, 0.6456)
            (430.0, 0.2839, 0.0116, 1.3856)
            (440.0, 0.3483, 0.0230, 1.7471)
            (450.0, 0.3362, 0.0380, 1.7721)
            (460.0, 0.2908, 0.0600, 1.6692)
            (470.0, 0.1954, 0.0910, 1.2876)
            (480.0, 0.0956, 0.1390, 0.8130)
            (490.0, 0.0320, 0.2080, 0.4652)
            (500.0, 0.0049, 0.3230, 0.2720)
            (510.0, 0.0093, 0.5030, 0.1582)
            (520.0, 0.0633, 0.7100, 0.0782)
            (530.0, 0.1655, 0.8620, 0.0422)
            (540.0, 0.2904, 0.9540, 0.0203)
            (550.0, 0.4334, 0.9950, 0.0087)
            (560.0, 0.5945, 0.9950, 0.0039)
            (570.0, 0.7621, 0.9520, 0.0021)
            (580.0, 0.9163, 0.8700, 0.0017)
            (590.0, 1.0263, 0.7570, 0.0011)
            (600.0, 1.0622, 0.6310, 0.0008)
            (610.0, 1.0026, 0.5030, 0.0003)
            (620.0, 0.8544, 0.3810, 0.0002)
            (630.0, 0.6424, 0.2650, 0.0000)
            (640.0, 0.4479, 0.1750, 0.0000)
            (650.0, 0.2835, 0.1070, 0.0000)
            (660.0, 0.1649, 0.0610, 0.0000)
            (670.0, 0.0874, 0.0320, 0.0000)
            (680.0, 0.0468, 0.0170, 0.0000)
            (690.0, 0.0227, 0.0082, 0.0000)
            (700.0, 0.0114, 0.0041, 0.0000)
            (710.0, 0.0058, 0.0021, 0.0000)
            (720.0, 0.0029, 0.0010, 0.0000)
            (730.0, 0.0014, 0.0005, 0.0000)
            (740.0, 0.0007, 0.0002, 0.0000)
            (750.0, 0.0003, 0.0001, 0.0000)
            (760.0, 0.0002, 0.0001, 0.0000)
            (770.0, 0.0001, 0.0000, 0.0000)
            (780.0, 0.0000, 0.0000, 0.0000)
        |]

    /// Relative spectral power of the standard illuminants at 10 nm: (λ nm, D65, A).
    let private illuminantTable : (float * float * float)[] =
        [|
            (380.0,  49.98,   9.80)
            (390.0,  54.65,  12.09)
            (400.0,  82.75,  14.71)
            (410.0,  91.49,  17.68)
            (420.0,  93.43,  21.00)
            (430.0,  86.68,  24.67)
            (440.0, 104.87,  28.70)
            (450.0, 117.01,  33.09)
            (460.0, 117.81,  37.81)
            (470.0, 114.86,  42.87)
            (480.0, 115.92,  48.24)
            (490.0, 108.81,  53.91)
            (500.0, 109.35,  59.86)
            (510.0, 107.80,  66.06)
            (520.0, 104.79,  72.50)
            (530.0, 107.69,  79.13)
            (540.0, 104.41,  85.95)
            (550.0, 104.05,  92.91)
            (560.0, 100.00, 100.00)
            (570.0,  96.33, 107.18)
            (580.0,  95.79, 114.44)
            (590.0,  88.69, 121.73)
            (600.0,  90.01, 129.04)
            (610.0,  89.60, 136.35)
            (620.0,  87.70, 143.62)
            (630.0,  83.29, 150.84)
            (640.0,  83.70, 157.98)
            (650.0,  80.03, 165.03)
            (660.0,  80.21, 171.96)
            (670.0,  82.28, 178.77)
            (680.0,  78.28, 185.43)
            (690.0,  69.72, 191.93)
            (700.0,  71.61, 198.26)
            (710.0,  74.35, 204.41)
            (720.0,  61.60, 210.36)
            (730.0,  69.89, 216.12)
            (740.0,  75.09, 221.67)
            (750.0,  63.59, 227.00)
            (760.0,  46.42, 232.12)
            (770.0,  66.81, 237.01)
            (780.0,  63.38, 241.68)
        |]

    /// Linear interpolation of a tabulated (λ nm, value) curve, clamped at the endpoints.
    let private interp (table : (float * float)[]) (wl : float) : float =
        let n = table.Length
        let (wl0, v0) = table.[0]
        let (wlN, vN) = table.[n - 1]
        if wl <= wl0 then v0
        elif wl >= wlN then vN
        else
            let mutable result = vN
            let mutable i = 0
            let mutable found = false
            while not found && i < n - 1 do
                let (wlA, vA) = table.[i]
                let (wlB, vB) = table.[i + 1]
                if wl >= wlA && wl <= wlB then
                    let f = if wlB = wlA then 0.0 else (wl - wlA) / (wlB - wlA)
                    result <- vA + f * (vB - vA)
                    found <- true
                else
                    i <- i + 1
            result

    let private xBarTable = cie1931 |> Array.map (fun (w, x, _, _) -> (w, x))
    let private yBarTable = cie1931 |> Array.map (fun (w, _, y, _) -> (w, y))
    let private zBarTable = cie1931 |> Array.map (fun (w, _, _, z) -> (w, z))
    let private d65Table = illuminantTable |> Array.map (fun (w, d, _) -> (w, d))
    let private aTable = illuminantTable |> Array.map (fun (w, _, a) -> (w, a))

    let private illuminantCurve =
        function
        | D65 -> d65Table
        | IlluminantA -> aTable

    /// §F.8 — integrate a chosen channel (R or T) over the sweep produced by
    /// `Variables.calculate` (whose first tuple element is the wavelength in nm) against
    /// the CIE 1931 2° CMFs and the chosen illuminant, returning CIE XYZ. The luminance Y
    /// is normalised so a perfect reflector (channel ≡ 1) yields the illuminant's white
    /// point (Y = 1). Consumes the §F.1 sweep output; does NOT re-run the solver.
    let spectrumToXyz
        (illuminant : Illuminant)
        (channel : OpticalFunction)
        (spectrum : (float * Solution)[])
        : float * float * float =

        let illum = illuminantCurve illuminant

        let samples =
            spectrum
            |> Array.map (fun (wl, sol) -> (wl, sol.func channel |> Option.defaultValue 0.0))

        let norm =
            samples
            |> Array.sumBy (fun (wl, _) -> interp illum wl * interp yBarTable wl)

        let k = if abs norm < 1.0e-300 then 0.0 else 1.0 / norm

        let x = k * (samples |> Array.sumBy (fun (wl, r) -> r * interp illum wl * interp xBarTable wl))
        let y = k * (samples |> Array.sumBy (fun (wl, r) -> r * interp illum wl * interp yBarTable wl))
        let z = k * (samples |> Array.sumBy (fun (wl, r) -> r * interp illum wl * interp zBarTable wl))
        (x, y, z)

    /// CIE D65 reference white point used by the Lab transform and assumed by the sRGB
    /// matrix below.
    let private d65White = (0.95047, 1.0, 1.08883)

    /// §F.8 — CIE XYZ → CIE L*a*b* (D65 reference white).
    let xyzToLab ((x, y, z) : float * float * float) : float * float * float =
        let (xn, yn, zn) = d65White
        let f t =
            if t > 0.008856 then t ** (1.0 / 3.0)
            else 7.787 * t + 16.0 / 116.0
        let fx = f (x / xn)
        let fy = f (y / yn)
        let fz = f (z / zn)
        let l = 116.0 * fy - 16.0
        let a = 500.0 * (fx - fy)
        let b = 200.0 * (fy - fz)
        (l, a, b)

    /// §F.8 — CIE XYZ → sRGB swatch (D65, standard matrix + gamma), clamped to [0, 1].
    let xyzToSrgb ((x, y, z) : float * float * float) : float * float * float =
        let r =  3.2406 * x - 1.5372 * y - 0.4986 * z
        let g = -0.9689 * x + 1.8758 * y + 0.0415 * z
        let b =  0.0557 * x - 0.2040 * y + 1.0570 * z
        let gamma c =
            let c = max 0.0 (min 1.0 c)
            if c <= 0.0031308 then 12.92 * c
            else 1.055 * (c ** (1.0 / 2.4)) - 0.055
        (gamma r, gamma g, gamma b)
