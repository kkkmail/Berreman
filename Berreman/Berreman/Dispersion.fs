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


    // ==========================================================================
    // Serializable eps tree (spec 0033 Part B).
    // Pure data from which toEpsWithDisp BUILDS the engine's EpsWithDisp above
    // through the existing Eps constructors; the engine unions stay unchanged.
    // ==========================================================================


    /// A non-dispersive eps — a DESCRIPTIVE DU, never a bare Eps: the case says
    /// what the medium is (symmetry × transparency), and toEps builds the engine
    /// matrix through the existing constructors. The uniaxial cases map to the
    /// (ordinary, extraordinary, ordinary) diagonal — the epsLa3Ga5SiO14 precedent.
    type ConstantEpsValue =
        | IsotropicTransparent of RefractionIndex
        | IsotropicAbsorbing of ComplexRefractionIndex
        | UniaxialTransparent of ordinary : RefractionIndex * extraordinary : RefractionIndex
        | UniaxialAbsorbing of ordinary : ComplexRefractionIndex * extraordinary : ComplexRefractionIndex
        | BiaxialTransparent of nx : RefractionIndex * ny : RefractionIndex * nz : RefractionIndex
        | BiaxialAbsorbing of nx : ComplexRefractionIndex * ny : ComplexRefractionIndex * nz : ComplexRefractionIndex

        member this.toEps : Eps =
            match this with
            | IsotropicTransparent n -> Eps.fromRefractionIndex n
            | IsotropicAbsorbing n -> Eps.fromComplexRefractionIndex n
            | UniaxialTransparent (nO, nE) -> Eps.fromRefractionIndex (nO, nE, nO)
            | UniaxialAbsorbing (nO, nE) -> Eps.fromComplexRefractionIndex (nO, nE, nO)
            | BiaxialTransparent (n1, n2, n3) -> Eps.fromRefractionIndex (n1, n2, n3)
            | BiaxialAbsorbing (n1, n2, n3) -> Eps.fromComplexRefractionIndex (n1, n2, n3)


    /// The dispersion of ONE principal axis: either separate real n and k
    /// formulas (k = the zero formula for a transparent medium), or one complex
    /// eps formula for inherently complex models (Lorentz / Drude).
    type EpsAxisDispersion =
        | RealNK of n : DispersionFormula * k : DispersionFormula
        | ComplexEps of ComplexDispersionFormula

        member this.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            match this with
            | RealNK (n, k) -> Complex (n.evaluate w, k.evaluate w) |> ComplexRefractionIndex
            | ComplexEps eps -> eps.evaluate w |> sqrt |> ComplexRefractionIndex


    /// One isotropic dispersion segment: a single axis over one interval.
    type IsotropicEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            dispersion : EpsAxisDispersion
        }


    /// One uniaxial dispersion segment: the interval is SHARED by both axes.
    type UniaxialEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            ordinaryDispersion : EpsAxisDispersion
            extraordinaryDispersion : EpsAxisDispersion
        }


    /// One biaxial dispersion segment: the interval is SHARED by all three axes.
    type BiaxialEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            xDispersion : EpsAxisDispersion
            yDispersion : EpsAxisDispersion
            zDispersion : EpsAxisDispersion
        }


    /// Selects the segment for a wavelength: the FIRST segment whose interval
    /// covers it (inclusive endpoints) — top-of-list wins on overlap; when none
    /// covers, the topmost segment extrapolates. No clamps, no validation.
    let private selectSegment (segments : 'S list) (intervalOf : 'S -> WaveLengthInterval) (w : WaveLength) : 'S =
        let covers s =
            let i = intervalOf s
            i.lower.value <= w.value && w.value <= i.upper.value
        match segments |> List.tryFind covers with
        | Some s -> s
        | None -> segments |> List.head


    /// A dispersive eps as data: a homogeneous segment list per symmetry.
    type EpsDispersiveValue =
        | IsotropicDispersive of IsotropicEpsSegment list
        | UniaxialDispersive of UniaxialEpsSegment list
        | BiaxialDispersive of BiaxialEpsSegment list

        member this.getEps (w : WaveLength) : Eps =
            match this with
            | IsotropicDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                s.dispersion.complexIndex w |> Eps.fromComplexRefractionIndex
            | UniaxialDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                let nO = s.ordinaryDispersion.complexIndex w
                let nE = s.extraordinaryDispersion.complexIndex w
                Eps.fromComplexRefractionIndex (nO, nE, nO)
            | BiaxialDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                Eps.fromComplexRefractionIndex (s.xDispersion.complexIndex w, s.yDispersion.complexIndex w, s.zDispersion.complexIndex w)


    /// The serializable counterpart of the engine's EpsWithDisp: either a
    /// dispersive segment tree or a constant value. toEpsWithDisp builds the
    /// engine type — the constant case short-circuits to EpsWithoutDisp.
    type EpsWithDispValue =
        | EpsWithDispValue of EpsDispersiveValue
        | EpsWithoutDispValue of ConstantEpsValue

        member this.toEpsWithDisp : EpsWithDisp =
            match this with
            | EpsWithDispValue d -> EpsWithDisp (fun w -> d.getEps w)
            | EpsWithoutDispValue c -> EpsWithoutDisp c.toEps


    // ==========================================================================
    // Serializable rho (gyration) tree (spec 0033 Part B).
    // Pure data — symmetry-class gyration ONLY (the rotation-producing classes;
    // deliberately no free 3x3 case). toRhoWithDisp lives in
    // OpticalProperties/Active.fs as a type extension: assembly needs the
    // crystal-class Rho builders there and the core cannot reference that project.
    // ==========================================================================


    /// The screw sense of an optically active crystal: the two enantiomorphs
    /// differ by ONE overall sign flip of the gyration tensor. RightHanded keeps
    /// the components as specified; LeftHanded negates the assembled tensor.
    type Handedness =
        | LeftHanded
        | RightHanded

        member this.sign : double =
            match this with
            | LeftHanded -> -1.0
            | RightHanded -> 1.0


    /// Uniaxial gyration components: the tensor is diag (g11, g11, g33).
    type UniaxialGyration<'g> =
        {
            g11 : 'g
            g33 : 'g
        }

        member this.map (f : 'g -> 'h) : UniaxialGyration<'h> =
            {
                g11 = f this.g11
                g33 = f this.g33
            }


    /// Orthorhombic class 222 gyration components: diag (g11, g22, g33).
    type Orthorhombic222Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
        }

        member this.map (f : 'g -> 'h) : Orthorhombic222Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
            }


    /// Monoclinic class 2 gyration components (two-fold axis along x2):
    /// the diagonal plus g13 = g31.
    type Monoclinic2Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
            g13 : 'g
        }

        member this.map (f : 'g -> 'h) : Monoclinic2Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
                g13 = f this.g13
            }


    /// Monoclinic class m gyration components (mirror normal to x2):
    /// only g12 = g21 and g23 = g32 survive.
    type MonoclinicMGyration<'g> =
        {
            g12 : 'g
            g23 : 'g
        }

        member this.map (f : 'g -> 'h) : MonoclinicMGyration<'h> =
            {
                g12 = f this.g12
                g23 = f this.g23
            }


    /// Triclinic class 1 gyration components: the full symmetric tensor.
    type Triclinic1Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
            g23 : 'g
            g13 : 'g
            g12 : 'g
        }

        member this.map (f : 'g -> 'h) : Triclinic1Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
                g23 = f this.g23
                g13 = f this.g13
                g12 = f this.g12
            }


    /// A symmetry-class gyration tensor over an abstract component 'g (RhoValue
    /// for a constant tensor, DispersionFormula for a dispersive one). ONLY the
    /// rotation-producing classes appear; the multi-component cases carry the
    /// NAMED records above, never anonymous tuples.
    type GyrationClass<'g> =
        | CubicActive of 'g
        | UniaxialActive of UniaxialGyration<'g>
        | PlanarActive of 'g
        | Orthorhombic222 of Orthorhombic222Gyration<'g>
        | Monoclinic2 of Monoclinic2Gyration<'g>
        | MonoclinicM of MonoclinicMGyration<'g>
        | Triclinic1 of Triclinic1Gyration<'g>

        member this.map (f : 'g -> 'h) : GyrationClass<'h> =
            match this with
            | CubicActive g11 -> CubicActive (f g11)
            | UniaxialActive u -> UniaxialActive (u.map f)
            | PlanarActive g12 -> PlanarActive (f g12)
            | Orthorhombic222 o -> Orthorhombic222 (o.map f)
            | Monoclinic2 m -> Monoclinic2 (m.map f)
            | MonoclinicM m -> MonoclinicM (m.map f)
            | Triclinic1 t -> Triclinic1 (t.map f)


    /// A gyration tensor plus the crystal's handedness — reused by both
    /// RhoWithDispValue cases.
    type GyrotropicValue<'g> =
        {
            gyration : GyrationClass<'g>
            hand : Handedness
        }


    /// The serializable counterpart of the engine's RhoWithDisp: a symmetry-class
    /// gyration whose components are either DispersionFormula (dispersive) or
    /// RhoValue (constant). toRhoWithDisp — the assembly onto the engine type —
    /// is a type extension in OpticalProperties/Active.fs, next to the
    /// crystal-class Rho builders it routes through.
    type RhoWithDispValue =
        | RhoWithDispValue of GyrotropicValue<DispersionFormula>
        | RhoWithoutDispValue of GyrotropicValue<RhoValue>


    // ==========================================================================
    // Serializable mu (Polder / gyromagnetic) tree (spec 0033 Part B).
    // Pure data + assembly: ONE generic Polder record covers both the constant
    // and dispersive cases, and toMuWithDisp BUILDS the engine's MuWithDisp
    // above through Mu.create — the engine unions stay unchanged. No solver
    // work: the Berreman matrix already reads off-diagonal mu.
    // ==========================================================================


    /// The magnetization axis of a gyromagnetic (Polder) permeability tensor.
    /// AlongZ is the Faraday geometry (propagation along the magnetization)
    /// and the default; the transverse axes (AlongX / AlongY) give the Voigt
    /// geometry.
    type GyrationAxis =
        | AlongX
        | AlongY
        | AlongZ

        static member defaultValue : GyrationAxis = AlongZ


    /// The Polder (gyromagnetic) permeability components over an abstract
    /// component 'g (MuValue for a constant tensor, DispersionFormula for a
    /// dispersive one): muDiagonal fills the two transverse diagonal slots,
    /// muParallel the axis slot, gyration the off-diagonal ±i·g pair. ONE
    /// generic record reused by both MuWithDispValue cases.
    type PolderValue<'g> =
        {
            muDiagonal : 'g
            muParallel : 'g
            gyration : 'g
            axis : GyrationAxis
        }

        member this.map (f : 'g -> 'h) : PolderValue<'h> =
            {
                muDiagonal = f this.muDiagonal
                muParallel = f this.muParallel
                gyration = f this.gyration
                axis = this.axis
            }


    /// Assembles the engine Mu for constant Polder components: in the AlongZ
    /// (Faraday) geometry the rows are [mu, +i·g, 0], [-i·g, mu, 0],
    /// [0, 0, muParallel]; the transverse (Voigt) axes are its cyclic
    /// permutations, so the ±i·g pair keeps its right-handed sense about the
    /// magnetization axis.
    let private polderMu (p : PolderValue<MuValue>) : Mu =
        let (MuValue mu) = p.muDiagonal
        let (MuValue muPar) = p.muParallel
        let (MuValue g) = p.gyration

        let m = Complex (mu, 0.0)
        let mPar = Complex (muPar, 0.0)
        let iG = Complex (0.0, g)
        let zero = Complex.Zero

        let rows =
            match p.axis with
            | AlongX ->
                [
                    [ mPar; zero; zero ]
                    [ zero; m; iG ]
                    [ zero; -iG; m ]
                ]
            | AlongY ->
                [
                    [ m; zero; -iG ]
                    [ zero; mPar; zero ]
                    [ iG; zero; m ]
                ]
            | AlongZ ->
                [
                    [ m; iG; zero ]
                    [ -iG; m; zero ]
                    [ zero; zero; mPar ]
                ]

        rows |> Mu.create


    /// A non-dispersive mu — a DESCRIPTIVE DU, never a bare Mu: either a
    /// scalar permeability (mu times the identity) or a constant gyromagnetic
    /// Polder tensor.
    type ConstantMuValue =
        | ScalarMu of MuValue
        | GyromagneticMu of PolderValue<MuValue>

        member this.toMu : Mu =
            match this with
            | ScalarMu (MuValue m) -> Complex (m, 0.0) * ComplexMatrix3x3.identity |> Mu
            | GyromagneticMu p -> polderMu p


    /// The serializable counterpart of the engine's MuWithDisp: either a
    /// dispersive Polder tensor or a constant value. toMuWithDisp builds the
    /// engine type — the constant cases short-circuit to MuWithoutDisp; the
    /// dispersive case evaluates each component's formula at the wavelength
    /// and assembles per call.
    type MuWithDispValue =
        | MuWithDispValue of PolderValue<DispersionFormula>
        | MuWithoutDispValue of ConstantMuValue

        member this.toMuWithDisp : MuWithDisp =
            match this with
            | MuWithDispValue p ->
                MuWithDisp (fun w -> p.map (fun (f : DispersionFormula) -> f.evaluate w |> MuValue) |> polderMu)
            | MuWithoutDispValue c -> MuWithoutDisp c.toMu
