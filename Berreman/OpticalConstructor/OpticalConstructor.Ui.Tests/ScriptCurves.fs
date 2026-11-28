namespace OpticalConstructor.Ui.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Analytics.Variables

/// A constructor's curve against its Analytics example script's: the script's points through `Variables.calculate`, and
/// the point-by-point comparison `FsxAcceptanceTests` and the reference kits' facts share (spec 0046; spec 0051
/// `.manual/007` 9.1).
module ScriptCurves =

    /// The script's data: `Variables.calculate` over the range, read through `fn` (what the script plots).
    let scriptData (info : FixedInfo) (range : RangedVariable) (fn : OpticalFunction) : (float * float) list =
        calculate info range
        |> Array.choose (fun p -> p.solution.func fn |> Option.map (fun y -> p.plotValue, y.value))
        |> List.ofArray

    /// The two curves agree point by point: the same x grid, and y within `tolerance` (absolute).
    let assertSameCurve (what : string) (tolerance : float) (expected : (float * float) list) (actual : (float * float) list) : unit =
        Assert.Equal(List.length expected, List.length actual)
        List.zip expected actual
        |> List.iter (fun ((xe, ye), (xa, ya)) ->
            Assert.True(abs (xe - xa) < 1.0e-9, $"%s{what}: x differs (%g{xe} vs %g{xa})")
            Assert.True(abs (ye - ya) <= tolerance, $"%s{what} at x = %g{xe}: script %.9f{ye}, constructor %.9f{ya}"))

    /// `MultilayerThinFilm_02.fsx`'s system: 100 quarter-wave pairs at 600 nm of n = 1.75 and n = 1.50 glass, under
    /// vacuum, on n = 1.50 glass, as the script builds it.
    let multilayerGlassPairsSystem : OpticalSystemWithDisp =
        let p12 =
            [
                { properties = OpticalProperties.transparentGlass175; thickness = Thickness.nm ((600.0 / 1.75 / 4.0) * 1.0<nm>) }
                { properties = OpticalProperties.transparentGlass150; thickness = Thickness.nm ((600.0 / 1.50 / 4.0) * 1.0<nm>) }
            ]
        let system : BaseOpticalSystem =
            {
                description = Some (OpticalSystemDescription "Multilayer thin film system.")
                upper = OpticalProperties.vacuum
                films = [ for _ in 1 .. 100 -> p12 ] |> List.concat
                lower = OpticalProperties.transparentGlass150
            }
        system.fullSystem.dispersive

    /// The incidence range the scripts plot, 0…89° (`IncidenceAngle.maxValue`), 89 intervals: a point per degree.
    let incidence0to89 : RangedVariable = Range<_>.create (NumberOfPoints 89) IncidenceAngle.normal IncidenceAngle.maxValue |> IncidenceAngleRange

    /// A wavelength range over `lowNm`…`highNm` in `intervals` intervals (`intervals` + 1 points).
    let waveLengthRange (intervals : int) (lowNm : float) (highNm : float) : RangedVariable =
        Range<_>.create (NumberOfPoints intervals) (WaveLength.nm (lowNm * 1.0<nm>)) (WaveLength.nm (highNm * 1.0<nm>)) |> WaveLengthRange

    /// `LangasiteOnSilicon.fsx`'s 10 µm langasite.
    let langasiteThickness : Thickness = Thickness.mm 0.01<mm>

    /// The largest gap between two curves on one x grid (absolute).
    let largestGap (expected : (float * float) list) (actual : (float * float) list) : float =
        List.zip expected actual |> List.fold (fun gap ((_, ye), (_, ya)) -> max gap (abs (ye - ya))) 0.0

    /// `system` with every langasite layer over `langasite` and the lower medium `silicon`: the script's film or plate
    /// over the built-in catalogue's materials, whose langasite keeps a constant ρ where the script's has the published
    /// law (`MaterialComplexityTests`; spec 0040 step 004, 0050 steps 10 and 51).
    let overCatalogueMaterials (langasite : OpticalPropertiesWithDisp) (silicon : OpticalPropertiesWithDisp) (system : OpticalSystemWithDisp) : OpticalSystemWithDisp =
        {
            system with
                filmsWithDisp = system.filmsWithDisp |> List.map (fun film -> { film with propertiesWithDisp = langasite })
                substrateWithDisp =
                    system.substrateWithDisp
                    |> Option.map (function
                        | PlateWithDisp plate -> PlateWithDisp { plate with propertiesWithDisp = langasite }
                        | WedgeWithDisp wedge -> WedgeWithDisp { wedge with layerWithDisp = { wedge.layerWithDisp with propertiesWithDisp = langasite } })
                lowerWithDisp = silicon
        }
