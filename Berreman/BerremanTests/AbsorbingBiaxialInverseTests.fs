namespace BerremanTests

open System.Numerics
open Berreman.Constants                                // the nm / mkm / mm units of measure
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerReconstruction   // Retardance — reused, not re-declared
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization
open Xunit
open BerremanTests.InverseFitHarness
open BerremanTests.BiaxialSample                       // the transparent twin, for the geometry and the scale
open BerremanTests.AbsorbingBiaxialSample              // the fifteen-parameter sample, shared with the noisy suite

/// Spec 0044, manual task 016 part 2 — the inverse problem for an ABSORBING, gyrotropic, TRICLINIC
/// crystal: FIFTEEN independent optical constants recovered at once, against the transparent twin's
/// nine.
///
/// THE COUNT.
///
///   * eps becomes COMPLEX: eps = eps' + i eps'', a complex symmetric 3x3, twelve real numbers. The
///     crystal frame is DEFINED as the one diagonalizing eps', which spends three of them on the frame
///     itself and leaves n1, n2, n3 — so eps' contributes 3 and eps'' contributes all SIX of its
///     components. Nine for the permittivity alone.
///
///   * THE TWO PARTS DO NOT SHARE PRINCIPAL AXES, and that is the whole reason eps'' carries six numbers
///     rather than three. Neumann's principle constrains eps' and eps'' independently and identically:
///     orthorhombic and higher symmetry locks both onto the crystallographic axes, monoclinic shares one
///     axis and lets the other two rotate, and TRICLINIC shares nothing. A triclinic absorbing crystal
///     generically has NO frame diagonalizing both — the observable consequence being the "dispersion of
///     the extinction directions" low-symmetry absorbing crystals are known for. Parameterizing eps'' by
///     its six components IN THE eps' FRAME captures that exactly, and avoids the gauge degeneracy and
///     periodicity three Euler angles for a second principal frame would drag into a fit. Measured here:
///     the principal absorption axes sit 42.2, 42.8 and 26.5 deg from the crystal axes.
///
///   * The GYRATION tensor contributes its usual SIX components, and NOT twelve. Textbook optics says it
///     goes complex too — g = g' + i g'', with g'' driving circular dichroism — and that is true of the
///     physics. It is not true of this engine, and the difference is measured rather than assumed: the
///     engine's rho is the bi-anisotropic magnetoelectric tensor, its IMAGINARY part drives optical
///     rotation at first order, and its REAL part has NO first-order effect at all. A thousandfold
///     increase in a real rho multiplies the resulting circular dichroism by 1.7e7, not by 1000. At
///     realistic gyration magnitudes (~1e-5) that leaves ~1e-13 of circular dichroism, i.e. nothing, and
///     six numerically dead columns would make the fit singular. So they are not carried, and the fact
///     that establishes it is the first inverse fact in this file.
///
/// 3 + 6 + 6 = 15.
///
/// THE MEASUREMENT SET HAD TO CHANGE, and not by adding geometries. A general Jones matrix carries 8
/// independent real parameters; forming a Mueller matrix loses the absolute phase, leaving 7;
/// NORMALIZING that Mueller matrix by m00 loses the absolute intensity, leaving 6. The quantity
/// normalization discards is precisely the ISOTROPIC ABSORPTION — the attenuation common to every
/// polarization state — and for an absorbing sample that is a real material parameter, not the source
/// brightness and detector gain it removes from a transparent one.
///
/// So this suite fits ABSOLUTE Mueller data: the fifteen normalized elements per observation plus one
/// log-throughput term. The identifiability fact below measures what that buys, and the answer is more
/// interesting than the argument predicts. Taken literally the information count says normalized data
/// should be BLIND to isotropic absorption; measured, it is 3.0x less sensitive to the isotropic
/// direction than to an anisotropic one, and absolute throughput restores a factor of 3.1. Weak, not
/// dead — because a PLATE is not a single pass, and its Fresnel coefficients and internally-reflected
/// beams both depend on the absorption level in a way normalization does not remove.
///
/// The isotropic PHASE is a different matter: unrecoverable from any intensity-based polarimetry,
/// because absolute phase is lost in forming a Mueller matrix from a Jones matrix. It is not a parameter
/// here and no amount of extra data would make it one.
///
/// THE SAMPLE. The transparent twin is `BiaxialSample` — the LBO-anchored triclinic constants
/// `BiaxialInverseTests` and `FeasibleBiaxialInverseTests` measure. This suite keeps its real part
/// exactly and adds the absorption, so the two are the same crystal at two ends of an absorption edge.
/// eps'' of order 1e-3 is k ~ 3e-4, alpha ~ 80 cm^-1, and a measured 24-47 % throughput across the
/// configuration set — squarely the "near the absorption edge but still measurable in transmission"
/// regime the literature puts at alpha ~ 100 cm^-1.
///
/// THE PLATE AND THE CUTS come from `FeasibleBiaxialInverseTests`: 100 um — the vendor minimum for LBO —
/// at a single wavelength, with optic-axis cuts as the gyration anchor. A caveat that suite does not
/// have: in an absorbing crystal the two optic axes SPLIT into four singular optical axes, so the
/// transparent twin's binormal is only approximately retardance-free here. At eps''/eps' ~ 1e-3 the
/// splitting is far below the alignment tolerance the plate already carries, but the cut is no longer
/// exactly an optic-axis cut.
type AbsorbingBiaxialInverseTests() =

    let degree = Berreman.MathNetNumericsMath.degree

    // =================================================================================================
    // The ground truth, the tensor builder, the fit scaling and the measurement set all live in
    // `AbsorbingBiaxialSample`, because `AbsorbingBiaxialNoisyTests` measures the same crystal with the
    // same experiment and differs only in whether the data carries measurement error.
    // =================================================================================================

    /// The magnitude used ONLY by the convention fact, to probe what the REAL part of rho does. It is not
    /// a fitted parameter: that fact measures it to be a second-order effect, and six parameters whose
    /// first-order influence is zero would make the fit singular.
    let dichroicProbe = RhoValue 1.90e-5

    let forward = createBerremanForward buildAbsorbing solverParameters

    let observe (configurations : MeasurementConfiguration list) : MuellerObservation list =
        observeWith forward absorbing configurations

    let box = searchBox

    /// The direction in parameter space that ADDS A CONSTANT to the ε'' diagonal — i.e. raises the
    /// isotropic absorption while leaving every anisotropy untouched. This is the null direction the
    /// information count predicts for normalized data, and it is a COMBINATION of three parameters
    /// rather than any one of them, which is why a Jacobian column norm cannot see it.
    let isotropicAbsorption (scaling : ParameterScaling<AbsorbingTriclinicParameters>) : float[] =
        sumWeights scaling [ "e11"; "e22"; "e33" ]

    /// A purely ANISOTROPIC absorption direction, for contrast: it raises ε''₁₁ and lowers ε''₂₂ by the
    /// same amount, so the isotropic level is unchanged and only the linear dichroism moves. Normalized
    /// data must see this one perfectly well, which is what makes the null-direction claim specific to
    /// the isotropic part rather than a general statement about absorption.
    let anisotropicAbsorption (scaling : ParameterScaling<AbsorbingTriclinicParameters>) : float[] =
        let w = Array.zeroCreate scaling.dimension
        let index (name : string) = scaling.axes |> List.findIndex (fun a -> a.name.value = name)
        w.[index "e11"] <- 1.0
        w.[index "e22"] <- -1.0
        w

    // =================================================================================================
    // The forward-side facts.
    // =================================================================================================

    [<Fact>]
    member _.``the scaled parameter space round-trips all fifteen coordinates and a unit step moves exactly one`` () =
        // The guard on fifteen hand-written axes. Each carries a `read` and a `write`, and a `read`
        // pointed at the wrong field, two axes sharing a `write`, or a transposition anywhere in the list
        // would produce a mapping that is smooth, total and WRONG — and a fit over a wrong mapping
        // converges confidently to a wrong answer instead of failing. At nine parameters that risk was
        // worth a fact; at fifteen it is not optional.
        let scaling = absorbingScalingAround absorbing
        Assert.Equal(15, scaling.dimension)
        Assert.Equal<float[]>(Array.zeroCreate 15, toScaled scaling absorbing)

        // A unit step in each scaled coordinate must move exactly that ONE parameter, by exactly its
        // scale, and leave the other fourteen untouched. All 225 (axis, parameter) pairs are checked.
        for (i, axis) in List.indexed scaling.axes do
            let step = Array.init 15 (fun k -> if k = i then 1.0 else 0.0)
            let moved = ofScaled scaling step
            for (k, other) in List.indexed scaling.axes do
                let before = other.read absorbing
                let after = other.read moved
                let expected = if k = i then before + other.read scaling.scale else before
                Assert.True(
                    abs (after - expected) < 1.0e-12 * max 1.0 (abs expected),
                    $"a unit step in {axis.name.value} moved {other.name.value} to {after}, expected {expected}")

    [<Fact>]
    member _.``absorption and refraction do NOT share principal axes, which is what makes this crystal triclinic-absorbing`` () =
        // The physical claim the six-component ε'' exists to express, asserted as an ANGLE.
        //
        // ε' is diagonal in the crystal frame by definition. ε'' is a real symmetric tensor too, so it has
        // its own orthonormal principal frame — and in a triclinic crystal there is no reason for the two
        // to coincide. Nothing in the parameterization forces them apart either: the off-diagonal ε''
        // components are free parameters, and setting them to zero would put the two frames back together
        // and quietly reduce this to a much easier, orthorhombic-like problem while still looking general.
        // So the misalignment is measured, and asserted to be substantial.
        let epsImaginary =
            [ [ epsIm11.value; epsIm12.value; epsIm13.value ]
              [ epsIm12.value; epsIm22.value; epsIm23.value ]
              [ epsIm13.value; epsIm23.value; epsIm33.value ] ]

        // The eigenvectors of ε'' are its principal absorption axes. Reached through the engine's own
        // matrix seam rather than hand-rolled.
        let (RealMatrix backing) = RealMatrix.create (epsImaginary |> List.map Array.ofList |> Array.ofList)
        let evd = backing.Evd()

        // The angle between each principal absorption axis and the nearest crystal axis. If the two
        // frames coincided every one of these would be zero.
        let misalignments =
            [ for column in 0 .. 2 ->
                let v = [ for row in 0 .. 2 -> evd.EigenVectors.[row, column] ]
                let largest = v |> List.map abs |> List.max
                acos (min 1.0 largest) / degree ]
        let report = System.String.Join("; ", [ for m in misalignments -> $"{m} deg" ])

        // Bands pinned per the spec §9 protocol from the observed 42.20, 42.77 and 26.51 deg. Two of the
        // three principal absorption axes sit more than 40 deg from any crystal axis: this is a strongly
        // triclinic-absorbing crystal, not one that is nominally general and numerically almost
        // orthorhombic.
        Assert.True(List.max misalignments > 20.0, $"the absorption axes must be misaligned from the crystal axes: {report}")

        // And ε'' must be a physically admissible absorption tensor — positive definite, i.e. the medium
        // absorbs in every polarization state rather than amplifying in some.
        let eigenvalues = [ for i in 0 .. 2 -> evd.EigenValues.[i].Real ]
        Assert.True(
            eigenvalues |> List.forall (fun e -> e > 0.0),
            $"""the absorption tensor must be positive definite, got {System.String.Join(", ", eigenvalues)}""")

    [<Fact>]
    member _.``the REAL part of rho has no first-order effect, so circular dichroism is not expressible through it`` () =
        // THE FINDING THAT SET THIS SUITE'S PARAMETER COUNT, and it is a negative one.
        //
        // Textbook optics says that for an absorbing gyrotropic crystal the gyration tensor goes complex:
        // g = g' + i g'', with g' driving circular BIREFRINGENCE (optical rotation) and g'' driving
        // circular DICHROISM. Carried into the engine's rho — whose transparent gyration is purely
        // IMAGINARY, since every crystal-class builder in `Active.fs` goes through `Rho.fromIm` — that
        // predicts the imaginary part of rho gives rotation and the real part gives circular dichroism.
        // Twelve gyration parameters rather than six.
        //
        // The first half is true. The second is NOT, and this fact measures it: a real symmetric rho has
        // NO FIRST-ORDER EFFECT AT ALL in this engine. Its influence is QUADRATIC, so at realistic
        // gyration magnitudes (~1e-5) it produces a circular dichroism of ~1e-13 — nothing. Six
        // parameters whose columns are numerically dead would make the fit singular, so this suite
        // carries fifteen unknowns and not twenty-one, and says why.
        //
        // The probes bypass the parameter record and build rho directly, because the real part is no
        // longer a fitted parameter and should not be given a slot that implies it could be.
        let complexOf (re : double) (im : double) = Complex(re, im)
        let transparentEps =
            Eps.fromRefractionIndex (index1, index2, index3)

        let probeMaterial (realScale : double) (imaginaryScale : double) : OpticalProperties =
            let entry (g : RhoValue) = complexOf (realScale * g.value) (imaginaryScale * g.value)
            {
                eps = transparentEps
                mu = Mu.vacuum
                rho =
                    [ [ entry g11; entry g12; entry g13 ]
                      [ entry g12; entry g22; entry g23 ]
                      [ entry g13; entry g23; entry g33 ] ]
                    |> Rho.create
            }

        let probe = configuration opticAxisCut 0.0 0.0 TransmittedMueller
        let coefficientsOf (properties : OpticalProperties) =
            let proxy = createBerremanForward (fun (_ : unit) -> properties) solverParameters
            match proxy.muellerOf probe () with
            | Ok m ->
                match analyticInversion m with
                | Ok a -> a
                | Error e -> failwith $"the analytic inversion failed: %A{e}"
            | Error e -> failwith $"the forward model failed: %A{e}"

        // Purely imaginary rho — the transparent gyration this repository has always used.
        let fromImaginary = coefficientsOf (probeMaterial 0.0 1.0)

        // Purely real rho of the SAME magnitude — what the textbook g'' would map onto.
        let fromReal = coefficientsOf (probeMaterial 1.0 0.0)

        // And the same real rho a thousand times larger. A weak-but-real first-order effect scales by
        // 1000; a quadratic one scales by 1000000; a structural zero does not scale at all. This is what
        // distinguishes the three, in one measurement.
        let fromStrongReal = coefficientsOf (probeMaterial 1000.0 0.0)

        let report =
            $"imaginary rho: cb {fromImaginary.cb.degrees} deg, cd {fromImaginary.cd.value} || "
            + $"real rho: cb {fromReal.cb.degrees} deg, cd {fromReal.cd.value} || "
            + $"real rho x1000: cb {fromStrongReal.cb.degrees} deg, cd {fromStrongReal.cd.value}, "
            + $"lb {fromStrongReal.lb.degrees} deg"

        // The IMAGINARY part behaves exactly as the transparent suites rely on: it produces circular
        // birefringence and no circular dichroism whatever.
        // Bands pinned per the spec §9 protocol from the observed values.
        // Measured: cb = 5.744 deg and cd = 1.8e-15, i.e. rotation and nothing else.
        Assert.True(abs fromImaginary.cb.degrees > 5.0, $"the imaginary part of rho must produce circular birefringence: {report}")
        Assert.True(abs fromImaginary.cd.value < 1.0e-12, $"the imaginary part of rho must NOT produce circular dichroism: {report}")

        // The REAL part produces neither, at its own magnitude: cb = 2.4e-15 and cd = 1.3e-12, both at
        // the solver's numerical floor.
        Assert.True(abs fromReal.cb.degrees < 1.0e-12, $"a real rho must produce no circular birefringence: {report}")
        Assert.True(abs fromReal.cd.value < 1.0e-10, $"a real rho must produce no first-order circular dichroism: {report}")

        // And the scaling settles WHY. A thousandfold increase multiplies the circular dichroism by
        // 1.7e7 — far past the thousandfold a first-order coupling would give, and consistent with a
        // quadratic one whose small-signal value is itself lost in the numerical floor. Whatever the
        // exact power, it is NOT first order, and that is what makes the six would-be g'' parameters
        // unusable.
        let growth = abs fromStrongReal.cd.value / abs fromReal.cd.value
        Assert.True(growth > 1.0e5, $"the real part of rho must not couple at FIRST order, growth = {growth}: {report}")

    [<Fact>]
    member _.``the absorbing sample attenuates, and every configuration stays physically realizable`` () =
        // Two invariants. The sample must actually absorb — otherwise ε'' is doing nothing and the whole
        // suite is the transparent one in disguise — and every Mueller matrix must remain physically
        // realizable, which for an absorbing medium is a sharper test than for a transparent one because
        // an error in the sign of ε'' produces GAIN, and gain is exactly what a Cloude eigenvalue catches.
        let observations = observe fullConfigurations
        Assert.True(observations.Length > 30, $"the measurement set should be substantial, got {observations.Length}")

        let transmittances =
            [ for o in observations do
                if o.configuration.observable = TransmittedMueller then
                    Propagation.muellerElement o.measured 0 0 ]
        let report = $"throughput from {List.min transmittances} to {List.max transmittances}"

        // Bands pinned per the spec §9 protocol from the observed values.
        // Measured: 24 % to 47 % throughput across the set, which is exactly the "near the absorption
        // edge but still comfortably measurable in transmission" regime the design was aimed at. Bands
        // pinned per the §9 protocol.
        Assert.True(List.max transmittances < 0.9, $"an absorbing plate must not transmit everything: {report}")
        Assert.True(List.min transmittances > 0.05, $"the plate must not be opaque either, or there is no data: {report}")

        for o in observations do
            let smallest = cloudeEigenvalues o.measured |> List.min
            Assert.True(
                smallest > -1.0e-9,
                $"%A{o.configuration.cut} at {o.configuration.incidenceAngle.value / degree} deg: negative Cloude eigenvalue {smallest}")

    // =================================================================================================
    // The inverse facts.
    // =================================================================================================

    [<Fact>]
    member _.``normalized data is markedly blind to isotropic absorption, and absolute throughput restores it`` () =
        // THE MEASUREMENT-SET FINDING, and the reason this suite fits absolute rather than normalized
        // Mueller matrices.
        //
        // The information count says a normalized Mueller-Jones matrix carries 6 of the 8 elementary
        // effects, and that the two it drops are the isotropic phase and the isotropic ABSORPTION. That
        // is an argument, and taken literally it predicts a HARD NULL. This is the measurement: take the
        // direction in parameter space that raises all three ε'' diagonal components together — pure
        // isotropic absorption, no anisotropy — and ask how hard each residual pushes back along it.
        //
        // THE MEASURED ANSWER IS SOFTER THAN THE ARGUMENT, and the difference is physical rather than
        // numerical. Normalized data is 3.0x less sensitive to the isotropic direction than to an
        // anisotropic one of the same size, and adding absolute throughput restores a factor of 3.1 —
        // but the isotropic direction is not dead under normalization, it is merely weak. The reason is
        // the same second-order channel the uniaxial suite records for its ordinary index: a PLATE is
        // not a single pass. Its Fresnel coefficients and its internally-reflected beams both depend on
        // the absorption level, and their relative weights survive normalization. A single-pass sample
        // would show the hard null; a plate leaks the isotropic level back in through its own geometry.
        //
        // It has to be a DIRECTIONAL sensitivity rather than a Jacobian column norm, because the null
        // direction is a COMBINATION of three parameters and every one of those three is individually
        // alive: e11 alone changes the linear dichroism, and normalized data sees that perfectly well.
        // Only the sum is invisible.
        let scaling = absorbingScalingAround perturbedStart
        let observations = observe fullConfigurations
        let truthPoint = toScaled scaling absorbing
        Assert.True(
            truthPoint |> Array.forall (fun x -> abs x > 1.0e-6),
            "the Jacobian must not be evaluated at the origin of the scaled space")

        let normalizedJacobian = FitQuality.residualJacobian (residualFor forward observations scaling) truthPoint
        let absoluteJacobian = FitQuality.residualJacobian (throughputResidualFor forward observations scaling) truthPoint

        let isotropic = isotropicAbsorption scaling
        let anisotropic = anisotropicAbsorption scaling

        let normalizedIsotropic = directionalSensitivity normalizedJacobian isotropic
        let absoluteIsotropic = directionalSensitivity absoluteJacobian isotropic
        let normalizedAnisotropic = directionalSensitivity normalizedJacobian anisotropic
        let report =
            $"isotropic: normalized {normalizedIsotropic}, absolute {absoluteIsotropic} || "
            + $"anisotropic under normalized data: {normalizedAnisotropic}"

        // Bands pinned per the spec §9 protocol from the observed 0.0677 (normalized, isotropic), 0.2086
        // (absolute, isotropic) and 0.2051 (normalized, anisotropic).
        Assert.True(normalizedIsotropic < 0.12, $"normalized data must be markedly blind to isotropic absorption: {report}")
        Assert.True(absoluteIsotropic > 0.12, $"absolute data must see isotropic absorption: {report}")
        Assert.True(
            absoluteIsotropic / normalizedIsotropic > 2.0,
            $"adding absolute throughput must materially improve the isotropic direction: {report}")

        // The contrast that makes the claim specific: an ANISOTROPIC absorption change of the same size
        // IS well seen by normalized data. It is the isotropic part alone that normalization suppresses,
        // which is what the information count predicts even though the suppression is a factor of three
        // rather than the total loss the count implies for a single-pass sample.
        Assert.True(
            normalizedAnisotropic / normalizedIsotropic > 2.0,
            $"normalized data must see anisotropic absorption markedly better than isotropic: {report}")

    [<Fact>]
    member _.``the absorbing measurement set recovers all FIFTEEN material constants from a perturbed start`` () =
        // THE DELIVERABLE of manual task 016 part 2: three principal refractive indices, six components
        // of the absorption tensor in a frame that does not diagonalize it, six gyration components
        // driving optical rotation and six more driving circular dichroism — twenty-one constants at
        // once, from Mueller data with absolute throughput.
        let observations = observe fullConfigurations
        let scaling = absorbingScalingAround perturbedStart
        let fit = fitAbsorbingObservations forward box observations scaling

        Assert.True(fit.solution.iterations > 0, "the fit should have taken at least one step")

        let errors = recoveryErrors scaling absorbing fit.recovered
        let report = $"{describeErrors errors}; chi2 {fit.chiSquared}; {fit.solution.iterations} iterations"

        // Bands pinned per the spec §9 protocol at ~29x the observed worst relative error of 3.5e-10
        // (g12) and ~16000x the observed final chi-squared of 6.4e-23. The three indices come back at
        // ~6e-15, the six absorption components at 8e-15 to 7e-13, and the six gyration components at
        // 1e-12 to 3.5e-10 — in 22 iterations, against the transparent nine-parameter fit's 28 on the
        // same plate and cuts. Adding absorption did not make the problem harder to solve; it added six
        // parameters that the linear dichroism constrains directly and well.
        for (name, err) in errors do
            Assert.True(err.value < 1.0e-8, $"{name.value} was not recovered: {report}")

        Assert.True(fit.chiSquared < 1.0e-18, $"final chi-squared out of band: {report}")

        // Guard against a vacuous pass, in all fifteen coordinates.
        let startErrors = recoveryErrors scaling absorbing perturbedStart
        Assert.True(
            startErrors |> List.forall (fun (_, e) -> e.value > 1.0e-4),
            $"the start guess must be outside the acceptance band everywhere: {describeErrors startErrors}")

    [<Fact>]
    member _.``all fifteen parameters are observable and the problem is conditioned well enough to solve`` () =
        // Identifiability at fifteen unknowns. Every parameter must move the residual, and the
        // condition number must be finite — the two statements that survive noiseless data, since
        // FitQuality's covariance underflows there (finding F3).
        let scaling = absorbingScalingAround perturbedStart
        let residual = throughputResidualFor forward (observe fullConfigurations) scaling
        let truthPoint = toScaled scaling absorbing
        let jacobian = FitQuality.residualJacobian residual truthPoint
        let norms = jacobianColumnNorms jacobian
        let report = describeColumnNorms scaling.axes norms

        for (i, axis) in List.indexed scaling.axes do
            Assert.True(norms.[i] > 1.0e-6, $"{axis.name.value} is unobservable in the absorbing set: {report}")

        let condition = jacobianCondition jacobian
        Assert.True(System.Double.IsFinite condition, $"the Jacobian condition number must be finite, got {condition}")
        // Measured 7677 — statistically indistinguishable from the 7954 the NINE-parameter transparent
        // fit shows on the same plate and cuts. Six extra unknowns cost essentially nothing in
        // conditioning, because the linear dichroism they carry is a channel the transparent problem
        // simply was not using: the six absorption columns come in at 0.19-0.24, an order of magnitude
        // STRONGER than any gyration column.
        Assert.True(condition < 2.0e4, $"Jacobian condition number = {condition}; {report}")
