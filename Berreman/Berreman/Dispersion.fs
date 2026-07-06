namespace Berreman
open System.Numerics
open Berreman.Geometry
open Constants
open MaterialProperties
open Fields
open Media

module Dispersion =

    type EpsWithDisp =
        | EpsWithDisp of (WaveLength -> Eps)
        | EpsWithoutDisp of Eps

        member this.getEps w =
            match this with
            | EpsWithDisp f -> f w
            | EpsWithoutDisp e -> e


    type Eps
        with
        member eps.dispersive = EpsWithoutDisp eps


    type MuWithDisp =
        | MuWithDisp of (WaveLength -> Mu)
        | MuWithoutDisp of Mu

        member this.getMu w =
            match this with
            | MuWithDisp f -> f w
            | MuWithoutDisp e -> e

    type Mu
        with
        member mu.dispersive = MuWithoutDisp mu


    type RhoWithDisp =
        | RhoWithDisp of (WaveLength -> Rho)
        | RhoWithoutDisp of Rho

        member this.getRho w =
            match this with
            | RhoWithDisp f -> f w
            | RhoWithoutDisp e -> e


    type Rho
        with
        member rho.dispersive = RhoWithoutDisp rho


    type OpticalPropertiesWithDisp =
        {
            epsWithDisp : EpsWithDisp
            muWithDisp : MuWithDisp
            rhoWithDisp : RhoWithDisp
        }

        member this.getProperties w =
            {
                eps = this.epsWithDisp.getEps w
                mu = this.muWithDisp.getMu w
                rho = this.rhoWithDisp.getRho w
            }


    type OpticalProperties
        with
        member this.dispersive =
            {
                epsWithDisp = this.eps.dispersive
                muWithDisp = this.mu.dispersive
                rhoWithDisp = this.rho.dispersive
            }


    type LayerWithDisp =
        {
            propertiesWithDisp : OpticalPropertiesWithDisp
            thickness : Thickness
        }

        member this.getLayer w =
            {
                properties = this.propertiesWithDisp.getProperties w
                thickness = this.thickness
            }

    type InclinedLayerWithDisp =
        {
            layerWithDisp : LayerWithDisp
            angle : WedgeAngle
        }

        member this.getInclinedLayer w v =
            {
                layer = this.layerWithDisp.getLayer w
                angle = v
            }


    type SubstrateWithDisp =
        | PlateWithDisp of LayerWithDisp
        | WedgeWithDisp of InclinedLayerWithDisp

        member this.getSubstrate w v =
            match this with
            | PlateWithDisp e -> e.getLayer w |> Plate
            | WedgeWithDisp e -> e.getInclinedLayer w v |> Wedge


    type Layer
        with
        member this.dispersive =
            {
                propertiesWithDisp = this.properties.dispersive
                thickness = this.thickness
            }


    type WedgeLayer
        with
        member this.dispersive =
            {
                layerWithDisp = this.layer.dispersive
                angle = this.angle
            }


    type Substrate
        with
        member this.dispersive =
            match this with
            | Plate e -> PlateWithDisp e.dispersive
            | Wedge e -> WedgeWithDisp e.dispersive


    type OpticalSystemWithDisp =
        {
            description : string option
            upperWithDisp : OpticalPropertiesWithDisp
            filmsWithDisp : List<LayerWithDisp>
            substrateWithDisp : SubstrateWithDisp option
            lowerWithDisp : OpticalPropertiesWithDisp
        }

        member this.getSystem w v =
            {
                description = this.description
                upper = this.upperWithDisp.getProperties w
                films = this.filmsWithDisp |> List.map (fun f -> f.getLayer w)
                substrate =
                    match this.substrateWithDisp with
                    | Some s -> s.getSubstrate w v |> Some
                    | None -> None
                lower = this.lowerWithDisp.getProperties w
            }

        member this.getWedgeAngle() =
            match this.substrateWithDisp with
            | Some (WedgeWithDisp w) -> Some w.angle
            | _ -> None


    type OpticalSystem
        with
        member this.dispersive =
            {
                description = this.description
                upperWithDisp = this.upper.dispersive
                filmsWithDisp = this.films |> List.map (fun f -> f.dispersive)
                substrateWithDisp =
                    match this.substrate with
                    | Some s -> s.dispersive |> Some
                    | None -> None
                lowerWithDisp = this.lower.dispersive
            }


    // ==========================================================================
    // Generic serializable dispersion formula blocks (spec 0033 Part B).
    // Pure data + evaluation from which closures BUILD the …WithDisp funcs above;
    // the engine unions in this file stay unchanged and non-serializable.
    // ==========================================================================


    /// A wavelength interval — pure serializable data: two elevated endpoints.
    /// An interval is just an interval: no ordering validation and no clamping —
    /// overlap ("first covering segment wins") and extrapolation ("topmost
    /// segment") rules belong to the consumers, not to the interval.
    type WaveLengthInterval =
        {
            lower : WaveLength
            upper : WaveLength
        }


    /// Integer power over Complex by repeated multiplication (Complex does not
    /// support pown); a negative exponent inverts, so a pole stays a pole —
    /// no clamping.
    let private complexPown (z : Complex) (n : int) : Complex =
        let p = { 1 .. abs n } |> Seq.fold (fun acc _ -> acc * z) Complex.One
        if n >= 0 then p else Complex.One / p


    /// One additive term of a real dispersion formula over the REDUCED
    /// wavelength x (the canonical wavelength divided by the owning formula's
    /// wavelengthScale): multiplier * (Σ_k coefficients.[k] * (x - lambda)^k) ^ power.
    /// A negative power expresses the inverse (Sellmeier / Laurent) shapes.
    type DispersionTerm =
        {
            lambda : double
            coefficients : double array
            power : int
            multiplier : double
        }

        /// Evaluates the term at the reduced wavelength x (already in coefficient units).
        member this.evaluate (x : double) : double =
            let d = x - this.lambda
            let poly = Array.foldBack (fun c acc -> acc * d + c) this.coefficients 0.0
            this.multiplier * pown poly this.power


    /// A real-valued dispersion formula: the sum of its terms over the reduced
    /// wavelength. wavelengthScale is METRES PER COEFFICIENT UNIT (e.g. 1.0e-6
    /// for coefficients tabulated in µm): evaluate reduces the canonical
    /// wavelength (metres) to the coefficient unit and sums the terms.
    type DispersionFormula =
        {
            terms : DispersionTerm list
            wavelengthScale : double
        }

        member this.evaluate (w : WaveLength) : double =
            let x = w.value / (this.wavelengthScale * 1.0<meter>)
            this.terms |> List.sumBy (fun t -> t.evaluate x)


    /// Mirrors DispersionTerm over System.Numerics.Complex — used ONLY where a
    /// model is inherently complex (e.g. Lorentz / Drude ε). The exponent stays
    /// an integer; the value fields (lambda, coefficients, multiplier) are complex.
    type ComplexDispersionTerm =
        {
            lambda : Complex
            coefficients : Complex array
            power : int
            multiplier : Complex
        }

        /// Evaluates the term at the reduced wavelength x (already in coefficient units).
        member this.evaluate (x : double) : Complex =
            let d = Complex (x, 0.0) - this.lambda
            let poly = Array.foldBack (fun c acc -> acc * d + c) this.coefficients Complex.Zero
            this.multiplier * complexPown poly this.power


    /// Mirrors DispersionFormula over System.Numerics.Complex; wavelengthScale
    /// stays real (METRES PER COEFFICIENT UNIT — a unit scale, not a value).
    type ComplexDispersionFormula =
        {
            terms : ComplexDispersionTerm list
            wavelengthScale : double
        }

        member this.evaluate (w : WaveLength) : Complex =
            let x = w.value / (this.wavelengthScale * 1.0<meter>)
            this.terms |> List.fold (fun acc t -> acc + t.evaluate x) Complex.Zero
