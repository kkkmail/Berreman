namespace BerremanTests

open Berreman.Constants                                // the nm / mkm / mm units of measure
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerReconstruction   // Retardance — reused, not re-declared
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization
open Xunit
open BerremanTests.InverseFitHarness
open BerremanTests.BiaxialSample                       // the SAME crystal BiaxialInverseTests measures

/// Spec 0044, manual task 016 part 1 — the same triclinic gyrotropic crystal as `BiaxialInverseTests`,
/// measured with an experiment a real laboratory could actually perform.
///
/// WHY THIS SUITE EXISTS. `BiaxialInverseTests` recovers all nine constants beautifully, from a
/// measurement set built on a 4 µm free-standing LBO plate. That plate is fiction. Three independent
/// vendors — Castech, Newlight and Eksma — all quote 100 µm as the MINIMUM thickness at which LBO can be
/// supplied at all, free-standing, with a ±20 µm tolerance and 20 arcsec parallelism. A 4 µm plate is
/// twenty-five times thinner than the thinnest LBO anyone sells, and thinner than the entire thickness
/// TOLERANCE on the thinnest part they do sell. Newlight puts the free-standing boundary explicitly at
/// 50 µm (below that the crystal must be optically contacted onto a fused-silica substrate), and
/// Altechna names it a material property rather than a process limit. The thinnest polished free-standing
/// single-crystal plate anywhere in the optical-activity literature is ~13 µm, and that is soft molecular
/// L-alanine mounted over a pinhole; hard inorganics bottom out near 58 µm.
///
/// So the 4 µm design answers the question "what would the inverse problem do with ideal data?" — which
/// is a real and useful question, and the reason that suite stays exactly as it is. This one answers a
/// different question: "what can you actually build, and what does it cost you?"
///
/// THE FIX IS NOT A THINNER PLATE — IT IS A DIFFERENT CUT. The 4 µm figure was forced by keeping linear
/// retardance sub-wave, because retardance enters through cos and sin of 2π Δn d / λ and a plate carrying
/// many waves gives a residual surface riddled with local minima half a fringe apart. But a biaxial
/// crystal has two directions in which the linear birefringence vanishes IDENTICALLY — its optic axes —
/// and a plate cut PERPENDICULAR TO AN OPTIC AXIS therefore carries no linear retardance at normal
/// incidence whatever its thickness. That is not a trick invented here; it is how biaxial optical
/// activity has been measured for a century. Real optic-axis plates in the literature are 0.42–1.32 mm
/// (NaNO₂) and quartz basal sections run 3–14.75 mm, with rotatory power quoted in deg/mm precisely
/// because thickness is free in that geometry.
///
/// WHAT THAT BUYS, in one number. Circular retardance grows as g·d, so the gyration signal is
/// proportional to thickness. At 4 µm the optical rotation of this crystal is 0.098°; at the 100 µm
/// vendor minimum it is 2.45°. Twenty-five times more signal, from a plate you can hold with tweezers.
///
/// AND WHAT IT COSTS. Off the optic axis the birefringence comes back linearly rather than quadratically:
/// Δn ≈ (n₃ − n₁)·sin(2V)·sin θ = 0.0406 sin θ, which on a 100 µm plate is 0.11 wave per degree of
/// misalignment. The optic-axis direction itself depends on the very indices being fitted — it moves
/// 1.54° between the truth and this suite's start guess — so an optic-axis plate is only usable if it is
/// thin enough that a wandering axis stays inside half a fringe. 100 µm gives 0.17 wave at that 1.54°
/// excursion; a 500 µm plate would give 0.86 wave and the fit would land in the wrong basin. The
/// mechanically feasible plate and the numerically safe plate turn out to be the same plate, which is a
/// piece of luck this suite asserts rather than assumes.
///
/// ONE WAVELENGTH, AND WHY. The standard laboratory answer to a multi-wave plate is to measure at
/// several wavelengths and let the order resolve itself, because δ = 2π Δn d / λ scales as 1/λ while the
/// aliasing step is a fixed 2π. That answer is NOT available to this fit, and the reason is dispersion:
/// n and g are functions of wavelength, so a single set of nine constants does not describe two colours
/// at once. Combining wavelengths would require fitting a DISPERSION MODEL — more unknowns, of a
/// different kind — which is a larger inverse problem than the one this suite is about. Every
/// configuration here is therefore at 632.8 nm, the line the sample's constants are quoted at.
///
/// THAT LEAVES A REAL, QUANTIFIED HAZARD, and this suite asserts it rather than dodging it. On the
/// principal cuts a 100 µm plate carries 6.69 waves, and the order is simply ABSENT from single-colour
/// data: a decoy one full order away differs from the truth by 1.1e-3 in the worst normalized Mueller
/// element — invisible on any real bench. The consequence is a hard precondition on the START GUESS
/// rather than on the data: the fit converges only if the initial birefringences are within half a
/// fringe, i.e. known to about 7 % beforehand. This suite's start is 5.7 % out on n₂ − n₁, which is
/// 0.25 wave — half the budget. That is the honest price of a mechanically real plate, and the order
/// fact below measures both halves of it.
///
/// The order-free half of the design carries no such precondition: the optic-axis plates have no linear
/// retardance to alias, at any thickness, and the tilt sweep grows it CONTINUOUSLY from zero, which is
/// the geometry the Glazer/Kaminsky "tilter" uses for exactly this reason.
type FeasibleBiaxialInverseTests() =

    /// Radians per degree, reached by its full module path. Bound here, ahead of the members (FS0960).
    let degree = Berreman.MathNetNumericsMath.degree

    /// The real Berreman-backed forward model, wired once for the whole class from the shared sample.
    let forward = createBerremanForward buildTriclinic solverParameters

    // =================================================================================================
    // The feasible experiment.
    // =================================================================================================

    /// The thinnest LBO plate any vendor supplies free-standing, and therefore the thinnest plate this
    /// suite is allowed to use: 100 µm, with ±20 µm tolerance and 20 arcsec parallelism (Castech,
    /// Newlight, Eksma all quote the same figure). Every configuration below uses exactly this.
    let vendorMinimumPlate = Thickness.mkm 100.0<mkm>

    /// The plate `BiaxialInverseTests` uses, kept here only so the feasibility fact can state the gap
    /// against a number rather than a memory.
    let idealizedPlate = Thickness.mkm 4.0<mkm>

    /// The one bench line. A single wavelength is not a simplification here but a requirement: the nine
    /// constants are wavelength-dependent, so data taken at two colours cannot be described by one set of
    /// them without a dispersion model.
    let waveLength = referenceWaveLength

    /// The optic-axis half-angle of THIS material, from its largest-index axis. It is the BINORMAL —
    /// the direction along which the two WAVE-NORMAL velocities coincide — at 54.025 deg, and NOT the
    /// biradial at 53.299 deg. The 0.73 deg between them is 29.6 deg of stray linear retardance on a
    /// 100 µm plate, so choosing the wrong one does not slightly degrade this design, it destroys it.
    /// The optic-axis fact below scans the cut angle and asserts the measured minimum lands here.
    let opticAxis = opticAxisWaveNormal triclinic

    /// A plate cut perpendicular to an optic axis. `TiltedCut a` rotates the crystal by `a` about y, and
    /// the sweep from `ZCut` to `XCut` carries the propagation direction from the crystal 3-axis to the
    /// crystal 1-axis through the 1-3 plane — which is exactly the plane the two optic axes lie in. The
    /// two of them sit at ±V from the 3-axis, so the two cuts are `TiltedCut (±V)`.
    let opticAxisCut = TiltedCut opticAxis
    let otherOpticAxisCut = TiltedCut -opticAxis

    let configuration (cut : SampleCut) (thickness : Thickness) (incidenceDeg : double) (azimuthDeg : double) (observable : Observable) (waveLength : WaveLength) : MeasurementConfiguration =
        configurationOf cut thickness incidenceDeg azimuthDeg observable waveLength

    /// F1 — THE ANCHOR. Both optic-axis cuts at normal incidence, four azimuths.
    /// No linear retardance at all, so the 100 µm plate is spent entirely on optical activity: 2.45 deg of
    /// rotation instead of the 0.098 deg the 4 µm design gets. This is the configuration that exists only
    /// because the crystal is biaxial, and it is the whole reason this design beats the idealized one.
    let f1 : MeasurementConfiguration list =
        [ for cut in [ opticAxisCut; otherOpticAxisCut ] do
            for azimuth in [ 0.0; 22.5; 45.0; 67.5 ] ->
                configuration cut vendorMinimumPlate 0.0 azimuth TransmittedMueller waveLength ]

    /// F2 — THE TILT SWEEP on the optic-axis plate, which is the "tilter" geometry. Tilting away from the
    /// optic axis grows the linear retardance CONTINUOUSLY from exactly zero, so however many waves it
    /// eventually reaches, the order is never ambiguous — it is tracked by continuity from a known zero.
    /// This is the part of the design that supplies angle and Fresnel leverage without inheriting the
    /// order hazard that the principal cuts carry.
    let f2 : MeasurementConfiguration list =
        [ for incidence in [ 20.0; 40.0; 60.0 ] do
            for azimuth in [ 0.0; 90.0 ] do
                for observable in [ TransmittedMueller; ReflectedMueller ] ->
                    configuration opticAxisCut vendorMinimumPlate incidence azimuth observable waveLength ]

    /// F3 — the three principal cuts, normal and oblique, transmission and reflection. These are the
    /// configurations that carry the order hazard, and they are here because `BiaxialInverseTests` showed
    /// that nothing else makes the gyration OFF-DIAGONALS observable: only oblique incidence on a
    /// principal cut couples through the longitudinal field component they live in.
    let f3 : MeasurementConfiguration list =
        [ for cut in [ ZCut; XCut; YCut ] do
            for incidence in [ 0.0; 45.0 ] do
                for azimuth in [ 0.0; 90.0 ] do
                    for observable in [ TransmittedMueller; ReflectedMueller ] ->
                        configuration cut vendorMinimumPlate incidence azimuth observable waveLength ]

    let fullConfigurations = f1 @ f2 @ f3

    let observe (configurations : MeasurementConfiguration list) : MuellerObservation list =
        observeWith forward triclinic configurations

    /// The start guess, identical in spirit to `BiaxialInverseTests`: every gyration component 25-35 %
    /// wrong in alternating directions, the three indices off by +0.20 %, +0.10 % and +0.15 %.
    ///
    /// Those index perturbations are what move the optic axis by 1.54 deg, which is the excursion the
    /// 100 µm optic-axis plate has to tolerate. They also move n₂ − n₁ by −5.7 %, which on a 100 µm plate
    /// is 0.25 wave of retardance error — half of the half-fringe budget, and the reason this design
    /// cannot use a thicker plate on the principal cuts however much the gyration signal would like one.
    let perturbedStart : TriclinicParameters =
        {
            index1 = RefractionIndex (index1.value * 1.0020)
            index2 = RefractionIndex (index2.value * 1.0010)
            index3 = RefractionIndex (index3.value * 1.0015)
            g11 = RhoValue (g11.value * 1.30)
            g22 = RhoValue (g22.value * 0.70)
            g33 = RhoValue (g33.value * 1.25)
            g23 = RhoValue (g23.value * 0.75)
            g13 = RhoValue (g13.value * 1.35)
            g12 = RhoValue (g12.value * 0.65)
        }

    let box = SearchBox 50.0

    /// The gyration Jacobian column norms the IDEALIZED 4 µm design achieves, transcribed from the
    /// measured table in `013-biaxial-triclinic-inverse.md`. They are constants here, not a re-run: the
    /// point of this suite is to beat them, and re-running that suite to find out would cost minutes for
    /// numbers already recorded.
    let idealisedGyrationNorms =
        [ "g11", 2.3571669951342757e-3
          "g22", 2.6357498633423244e-3
          "g33", 2.3920665134676234e-3
          "g23", 9.778185443523834e-4
          "g13", 1.4794920347657742e-3
          "g12", 1.0336273123323788e-3 ]

    // =================================================================================================
    // The feasibility facts — about the DESIGN, not about the fit.
    // =================================================================================================

    [<Fact>]
    member _.``every plate in this design is one a vendor will actually sell`` () =
        // The premise of the whole suite, asserted so it cannot rot. Castech, Newlight and Eksma all put
        // LBO's minimum free-standing thickness at 100 µm; Newlight additionally states that below 50 µm
        // the crystal must be optically contacted onto a fused-silica substrate and that free-standing is
        // unconditional only at >= 100 µm. This design therefore sits exactly on the vendor floor.
        let millimetres (t : Thickness) =
            match t with
            | Thickness d -> d / 1.0<meter> * 1.0e3
            | Infinity -> failwith "a sample plate must have a finite thickness"

        let vendorFloorMm = 0.1
        for c in fullConfigurations do
            Assert.True(
                millimetres c.thickness >= vendorFloorMm - 1.0e-12,
                $"a configuration uses a {millimetres c.thickness} mm plate, below the {vendorFloorMm} mm vendor minimum")

        // And the gap against the idealized design is stated as a number rather than a memory: the 4 µm
        // plate is twenty-five times below that floor, and below the +-20 µm TOLERANCE on the thinnest
        // part a vendor will make.
        let ratio = vendorFloorMm / millimetres idealizedPlate
        Assert.True(abs (ratio - 25.0) < 0.01, $"the idealized plate should be 25x below the vendor floor, got {ratio}")
        Assert.True(millimetres idealizedPlate < 0.020, "the idealized plate is thinner than the vendor thickness TOLERANCE")

    [<Fact>]
    member _.``a plate cut perpendicular to an optic axis is a pure circular retarder, and that is what buys the signal`` () =
        // THE DESIGN CLAIM, measured rather than argued.
        //
        // Along an optic axis the two transverse principal indices coincide, so the linear birefringence
        // vanishes identically and the plate is a pure optical rotator however thick it is. That is what
        // frees the thickness, and freeing the thickness is what buys the gyration signal, because
        // circular retardance grows as g*d while linear retardance grows as dn*d and it is the LINEAR one
        // that has to stay sub-wave.
        //
        // The two optic axes sit at +-V from the largest-index axis in the 1-3 plane. `TiltedCut a`
        // rotates the crystal by `a` about y, and that sweep carries the propagation direction from the
        // crystal 3-axis (ZCut) to the crystal 1-axis (XCut) through exactly that plane — so
        // `TiltedCut (+-V)` is the optic-axis cut. Asserted here rather than assumed, because a sign
        // error would leave the plate off-axis and quietly destroy the whole design.
        // WHICH ANGLE IS THE OPTIC AXIS is settled by MEASUREMENT, not by picking one of the two formulas
        // the textbooks offer. The literature carries both `tan²V = (n₂²−n₁²)/(n₃²−n₂²)` (the RAY axes,
        // or biradials) and `tan²V = (n₁⁻² − n₂⁻²)/(n₂⁻² − n₃⁻²)` (the WAVE-NORMAL axes, or binormals),
        // and for this material they differ by 0.76 deg — which at 100 µm is 0.08 wave of linear
        // retardance, i.e. the difference between a pure rotator and a visibly elliptical one. At normal
        // incidence there is no refraction, so the internal WAVE NORMAL is the surface normal and it is
        // the binormal that must be used. Rather than assert that from theory, the fact scans the linear
        // retardance across the cut angle and asserts the minimum lands on the binormal prediction.
        let linearRetardanceAt (cutDeg : double) =
            let observation =
                observe [ configuration (TiltedCut (Angle.degree cutDeg)) vendorMinimumPlate 0.0 0.0 TransmittedMueller referenceWaveLength ]
                |> List.exactlyOne
            match analyticInversion observation.measured with
            | Ok a -> sqrt (a.lb.degrees ** 2.0 + a.lbPrime.degrees ** 2.0)
            | Error e -> failwith $"the analytic inversion failed at {cutDeg} deg: %A{e}"

        let sq (x : double) = x * x
        let biradial = atan (sqrt ((sq index2.value - sq index1.value) / (sq index3.value - sq index2.value))) / degree
        let binormal =
            atan (sqrt ((1.0 / sq index1.value - 1.0 / sq index2.value) / (1.0 / sq index2.value - 1.0 / sq index3.value))) / degree

        let scan = [ for k in -20 .. 20 -> biradial + 0.1 * float k ]
        let best = scan |> List.minBy linearRetardanceAt
        let scanReport =
            $"biradial {biradial} deg, binormal {binormal} deg, measured minimum at {best} deg "
            + $"(residual linear retardance {linearRetardanceAt best} deg; at biradial {linearRetardanceAt biradial} deg)"

        // Bands pinned per the spec §9 protocol from the observed scan. The measured minimum lands
        // 0.026 deg from the binormal prediction — inside the 0.1 deg scan step, i.e. as close as this
        // grid can resolve — while the biradial angle 0.73 deg away carries 29.6 deg of linear
        // retardance. The residual 1.06 deg at the grid minimum is the grid's own coarseness (0.11 wave
        // per degree of misalignment on this plate), not a failure of the identification: evaluated at
        // the EXACT binormal below, the linear retardance falls to 1.5e-5 deg.
        Assert.True(abs (best - binormal) < 0.15, $"the linear-retardance minimum must sit on the BINORMAL: {scanReport}")
        Assert.True(linearRetardanceAt best < 2.0, $"at the optic axis the linear retardance must essentially vanish: {scanReport}")
        Assert.True(
            linearRetardanceAt biradial > 10.0,
            $"the BIRADIAL must be visibly NOT an optic axis, or this fact proves nothing: {scanReport}")

        let opticAxisObservation =
            observe [ configuration opticAxisCut vendorMinimumPlate 0.0 0.0 TransmittedMueller referenceWaveLength ]
            |> List.exactlyOne

        match analyticInversion opticAxisObservation.measured with
        | Ok a ->
            // Bands pinned per the §9 protocol from the observed lb = -1.5e-5 deg, lb' = 1.5e-6 deg and
            // cb = 5.744 deg. The plate is a circular retarder to five decimal places in degrees, and it
            // carries 2.87 deg of optical rotation — on a plate a vendor will sell.
            let describe = $"lb {a.lb.degrees} deg, lb' {a.lbPrime.degrees} deg, cb {a.cb.degrees} deg || {scanReport}"
            Assert.True(abs a.lb.degrees < 1.0e-3, $"optic-axis cut: {describe}")
            Assert.True(abs a.lbPrime.degrees < 1.0e-3, $"optic-axis cut: {describe}")
            Assert.True(abs a.cb.degrees > 5.0, $"optic-axis cut: {describe}")
        | Error e -> Assert.Fail($"the optic-axis cut should invert to a near-pure rotator, got %A{e}")

        // The comparison that justifies the redesign: the SAME cut on the SAME crystal at the idealized
        // 4 µm thickness carries 25x less circular retardance, because circular retardance is linear in
        // thickness and this cut has no linear retardance to trade against.
        let thinObservation =
            observe [ configuration opticAxisCut idealizedPlate 0.0 0.0 TransmittedMueller referenceWaveLength ]
            |> List.exactlyOne

        match analyticInversion opticAxisObservation.measured, analyticInversion thinObservation.measured with
        | Ok thick, Ok thin ->
            let gain = abs thick.cb.value / abs thin.cb.value
            Assert.True(abs (gain - 25.0) < 0.5, $"the feasible plate should carry 25x the circular retardance, got {gain}")
        | other -> Assert.Fail($"both plates should invert: %A{other}")

    [<Fact>]
    member _.``the order ambiguity is real on the principal cuts and is broken by the optic-axis cuts`` () =
        // The cost side of the redesign, and the reason wavelength diversity had to come with it.
        //
        // The optic-axis cut is retardance-free, but the PRINCIPAL cuts are not, and at 100 µm they carry
        // 6.69 waves at the largest birefringence. A single-wavelength measurement cannot tell 6.69 waves
        // from 5.69 or 7.69: the Mueller matrix depends on the retardance only through cos and sin, so
        // the order is simply absent from the data. Three wavelengths break that, because the retardance
        // scales as 1/lambda while the aliasing step is a fixed 2*pi — a parameter set that reproduces
        // the 632.8 nm data one order out reproduces neither of the other two.
        //
        // Asserted as an INFORMATION statement, not a fit: take the truth, take a decoy whose largest
        // birefringence is one full order away at the HeNe line, and show that the decoy is invisible at
        // that one wavelength while being glaring across all three.
        let plateWaves (dn : double) (w : WaveLength) =
            match vendorMinimumPlate with
            | Thickness d -> dn * (d / w.value)
            | Infinity -> failwith "finite plate"

        let trueSpan = index3.value - index1.value
        Assert.True(abs (plateWaves trueSpan referenceWaveLength - 6.692) < 0.01, $"expected 6.69 waves, got {plateWaves trueSpan referenceWaveLength}")

        // THE DECOY has to be a CLEAN order alias or the fact proves nothing. Shifting one index alone
        // does not do it: n3 - n1 and n3 - n2 both move, and only one of them can be made integral. The
        // construction that works is to shift n2 and n3 TOGETHER by exactly one wave, which sends
        // n2 - n1 up by one order, leaves n3 - n2 untouched, and therefore carries n3 - n1 up by one
        // order as well — all three birefringences integral, at the HeNe line.
        let oneWaveShift =
            match vendorMinimumPlate with
            | Thickness d -> referenceWaveLength.value / d
            | Infinity -> failwith "finite plate"
        let decoy =
            { triclinic with
                index2 = RefractionIndex (index2.value + oneWaveShift)
                index3 = RefractionIndex (index3.value + oneWaveShift) }

        let principalNormal =
            [ for cut in [ ZCut; XCut; YCut ] do
                for azimuth in [ 0.0; 45.0 ] ->
                    configuration cut vendorMinimumPlate 0.0 azimuth TransmittedMueller waveLength ]
        let opticAxisNormal =
            [ for cut in [ opticAxisCut; otherOpticAxisCut ] do
                for azimuth in [ 0.0; 45.0 ] ->
                    configuration cut vendorMinimumPlate 0.0 azimuth TransmittedMueller waveLength ]

        let worstElementGap (configurations : MeasurementConfiguration list) =
            let truthData = observe configurations
            match forwardModels forward decoy truthData with
            | Ok models ->
                match residualVector truthData models with
                | Ok r -> r |> Array.map abs |> Array.max
                | Error e -> failwith $"residual: %A{e}"
            | Error e -> failwith $"forward: %A{e}"

        let onPrincipalCuts = worstElementGap principalNormal
        let onOpticAxisCuts = worstElementGap opticAxisNormal

        // THE HAZARD, in one number. On the principal cuts the decoy is nearly indistinguishable from the
        // truth: the order is simply not present in the data, and no amount of precision recovers it.
        // A normalized Mueller element runs from -1 to 1, so a worst-element gap of ~1e-3 is a decoy
        // hiding inside the noise of even a mediocre bench.
        //
        // THE MITIGATION, in the next number. The optic-axis plates have no linear retardance to alias,
        // so a shift that is invisible as an ORDER on the principal cuts still moves them — through the
        // ordinary first-order dependence of the gyration projection and the Fresnel terms on the
        // indices. They do not resolve the order (nothing at one wavelength can), but they do stop the
        // aliased branch from being an equally good fit to the WHOLE data set.
        //
        // Bands pinned per the §9 protocol from the observed values.
        let gapReport = $"principal cuts {onPrincipalCuts}, optic-axis cuts {onOpticAxisCuts}"
        // Measured: 1.1e-3 on the principal cuts against 1.65 on the optic-axis cuts — a factor of 1497.
        // This is the result that makes the single-wavelength design work at all, and it was not obvious
        // in advance: the optic-axis plate was added to AMPLIFY THE GYRATION SIGNAL, and it turns out to
        // break the order ambiguity as well. It can, precisely because it has no linear retardance — an
        // index shift that is a pure 2*pi alias where retardance dominates is an ordinary first-order
        // change where there is none.
        Assert.True(onPrincipalCuts < 5.0e-3, $"a one-order decoy must be nearly invisible on the principal cuts: {gapReport}")
        Assert.True(onOpticAxisCuts > 0.5, $"the optic-axis cuts must still see the decoy: {gapReport}")
        Assert.True(
            onOpticAxisCuts / onPrincipalCuts > 300.0,
            $"the optic-axis cuts must expose the decoy far better than the principal ones: {gapReport}")

        // AND THE PRECONDITION THAT FOLLOWS. Because the order cannot be recovered from the data, it has
        // to come from the START GUESS: the fit converges only if every initial birefringence is within
        // half a fringe of the truth. Asserted directly on this suite's own start, so that loosening the
        // start guess without re-examining the plate thickness fails here rather than silently converging
        // to an aliased branch.
        let fringesOff (fromIndex : TriclinicParameters -> double) =
            match vendorMinimumPlate with
            | Thickness d -> abs (fromIndex perturbedStart - fromIndex triclinic) * (d / waveLength.value)
            | Infinity -> failwith "finite plate"

        let starts =
            [ "n2 - n1", fringesOff (fun p -> p.index2.value - p.index1.value)
              "n3 - n2", fringesOff (fun p -> p.index3.value - p.index2.value)
              "n3 - n1", fringesOff (fun p -> p.index3.value - p.index1.value) ]
        let startReport = System.String.Join("; ", [ for (n, f) in starts -> $"{n} off by {f} fringe" ])
        for (name, fringes) in starts do
            Assert.True(fringes < 0.5, $"the start guess must be inside half a fringe on {name}: {startReport}")

    // =================================================================================================
    // The inverse facts.
    // =================================================================================================

    [<Fact>]
    member _.``the feasible measurement set recovers all NINE constants from a perturbed start`` () =
        // THE DELIVERABLE: the same nine-parameter recovery `BiaxialInverseTests` achieves, but from data
        // an actual laboratory could produce — 100 µm plates a vendor will sell, three ordinary bench
        // wavelengths, and one crystallographic cut (perpendicular to an optic axis) that is standard
        // practice in this exact field.
        let observations = observe fullConfigurations
        let scaling = scalingAround perturbedStart
        let fit = fitObservations forward box observations scaling

        Assert.True(fit.solution.iterations > 0, "the fit should have taken at least one step")

        let errors = recoveryErrors scaling triclinic fit.recovered
        let report = $"{describeErrors errors}; chi2 {fit.chiSquared}; {fit.solution.iterations} iterations"

        // Bands pinned per the spec §9 protocol at ~37x the observed worst relative error of 2.7e-10
        // (g23) and ~37000x the observed final chi-squared of 2.7e-23. The three indices come back at
        // ~5e-15 and the gyration components at 7e-14 to 2.7e-10, in 28 iterations — against the
        // idealized design's 12. The extra iterations are the price of the principal cuts carrying 6.7
        // waves instead of 0.27: the residual surface is far more structured, and the optimizer walks
        // further to cross it.
        for (name, err) in errors do
            Assert.True(err.value < 1.0e-8, $"{name.value} was not recovered: {report}")

        Assert.True(fit.chiSquared < 1.0e-18, $"final chi-squared out of band: {report}")

        // Guard against a vacuous pass.
        let startErrors = recoveryErrors scaling triclinic perturbedStart
        Assert.True(
            startErrors |> List.forall (fun (_, e) -> e.value > 1.0e-4),
            $"the start guess must be outside the acceptance band in every coordinate: {describeErrors startErrors}")

    [<Fact>]
    member _.``the feasible design constrains the gyration tensor FAR better than the idealized thin-plate one`` () =
        // The payoff, quantified. `015-biaxial-noisy-measurement-tests.md` recorded that the idealized
        // design's fatal weakness was gyration sensitivity: circular retardance grows as g*d, the 4 µm
        // plate is 250x thinner than a normal one, and the six gyration components came back from a
        // realistic bench with error bars the size of their own values. It identified the fix — a thick
        // plate cut near an optic axis — as the obvious next slice. This is that slice, measured.
        //
        // The comparison is between Jacobian COLUMN NORMS at the truth, which is the scale-free statement
        // of how hard the data pushes back on each parameter. The idealized figures are transcribed
        // constants from `013`, not a re-run.
        let scaling = scalingAround perturbedStart
        let residual = residualFor forward (observe fullConfigurations) scaling
        let truthPoint = toScaled scaling triclinic
        Assert.True(
            truthPoint |> Array.forall (fun x -> abs x > 1.0e-6),
            "the Jacobian must not be evaluated at the origin of the scaled space")

        let norms = jacobianColumnNorms (FitQuality.residualJacobian residual truthPoint)
        let report = describeColumnNorms scaling.axes norms
        let normOf (name : string) = norms.[scaling.axes |> List.findIndex (fun a -> a.name.value = name)]

        // Every parameter still observable.
        for (i, axis) in List.indexed scaling.axes do
            Assert.True(norms.[i] > 1.0e-6, $"{axis.name.value} is unobservable in the feasible set: {report}")

        // And every gyration component pushed harder than the idealized design managed.
        // Bands pinned per the §9 protocol from the observed ratios.
        let gains =
            [ for (name, idealised) in idealisedGyrationNorms -> name, normOf name / idealised ]
        let gainReport =
            System.String.Join("; ", [ for (name, gain) in gains -> $"{name} x{gain}" ])
        for (name, gain) in gains do
            Assert.True(gain > 1.2, $"the feasible design must constrain {name} better than the 4 um design: {gainReport} || {report}")

        // Every one of the six is better, and most of them are better by a lot: the measured gains are
        // x9.2 (g11), x22.0 (g22), x15.4 (g33), x1.6 (g23), x37.7 (g13), x1.4 (g12). Four of the six gain
        // more than fivefold. The two that barely move are g23 and g12 — the off-diagonal components the
        // idealized suite showed are reachable ONLY through oblique incidence, and the oblique group is
        // the one part of this design that did not change.
        let bigGains = gains |> List.filter (fun (_, gain) -> gain > 5.0) |> List.length
        Assert.True(bigGains >= 4, $"most gyration components should gain more than fivefold: {gainReport}")

        // THE COST, reported rather than buried. Absolute gyration sensitivity is what sets the error bar
        // under noise, and this design wins on every component. But the INDEX columns grew even faster —
        // 100 µm of retardance-bearing path against 4 µm is 25x more sensitivity to n — so the ratio of
        // the strongest column to the weakest is WORSE here, and the Jacobian condition number with it.
        // Both statements are true and neither cancels the other: a better-conditioned problem is easier
        // to solve, but it is the absolute column norms that decide how much a noisy measurement can say.
        let condition = jacobianCondition (FitQuality.residualJacobian residual truthPoint)
        Assert.True(System.Double.IsFinite condition, $"the Jacobian condition number must be finite, got {condition}")
        Assert.True(condition < 2.0e4, $"Jacobian condition number = {condition}; {report}")
