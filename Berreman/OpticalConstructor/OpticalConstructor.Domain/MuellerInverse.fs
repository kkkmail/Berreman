namespace OpticalConstructor.Domain

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalConstructor.Domain.MuellerReconstruction   // Retardance (spec 0042) — reused, not re-declared

/// Spec 0044 — the inverse problem for a transparent, optically active, anisotropic, homogeneous
/// material: recover the material's optical constants from measured Mueller matrices.
///
/// The module is deliberately MATERIAL-AGNOSTIC (spec §5.1). `OpticalConstructor.Domain` references
/// `Berreman` and `Analytics` but not `OpticalProperties`, so nothing here names a crystal class or a
/// named material: the forward proxy takes a caller-supplied `'Parameters -> OpticalProperties` builder,
/// and the caller decides what physics that builder encodes. That keeps the module reusable for the next
/// material and avoids a project reference that would exist only to reach a handful of constants.
///
/// It is agnostic about the PARAMETER SET too, not merely about the material. Everything from the scaling
/// down — `ParameterScaling`, `toScaled`, `ofScaled`, `ForwardModelProxy`, `forwardModels`, and every
/// diagnostic built on the residual Jacobian — is generic in `'Parameters` and is driven by a list of
/// `ParameterAxis` values the material supplies. A four-unknown uniaxial fit and a nine-unknown triclinic
/// one therefore run the SAME code; only the axis list differs.
///
/// The physics the design turns on. A wave only ever sees the components of the material tensors that are
/// TRANSVERSE to its own propagation direction, so no single propagation direction can constrain the whole
/// tensor. For a uniaxial crystal:
///
///   • propagating ALONG the optic axis, the two transverse directions are equivalent — there is no linear
///     birefringence at all and the sample acts as a pure circular retarder;
///   • propagating ACROSS the optic axis, the transverse directions are the ordinary and extraordinary
///     ones — linear birefringence is maximal and buries the (much smaller) optical activity.
///
/// A second, sharper constraint: those two regimes CANNOT be reached from one sample by tilting it.
/// Snell's law caps the internal angle at `arcsin(1/n_o)` — about 40.4° for quartz — so a plate cut with
/// its optic axis along the normal never gets the wave more than 40° away from that axis, however far it
/// is tilted. Spanning the full range therefore REQUIRES more than one crystal cut, which is why
/// `OpticAxisCut` exists and why the measurement set is built the way it is. The ablation fact in the test
/// suite asserts the resulting unobservability directly rather than assuming it.
///
/// ---
///
/// A CONVENTION WARNING that anyone reading gyration values out of this pipeline must have, established by
/// measurement against the engine and asserted by the forward-model test:
///
/// The engine's `Rho` is the BI-ANISOTROPIC (Tellegen–Post) magnetoelectric tensor of `D = εE + ρH`, NOT
/// the crystallographic gyration tensor `g` of `D = εE + i(G × E)`. The two share the symbol `g` in the
/// literature and in `Active.Rho.type_3_4_6_Crystal`, but they are different parameterizations, and for
/// propagation along z the engine's behaviour is exactly
///
///     Δn_circular = 2·ρ_transverse          →  specific rotation = 2π·g₁₁ / λ
///
/// — driven by the TRANSVERSE component `g₁₁`, independent of `g₃₃` and independent of the refractive
/// index. The crystallographic convention would instead give `π·g₃₃ / (n·λ)`. Concretely, with quartz's
/// literature constants placed in the engine's `Rho` slots, a z-cut plate rotates at 33.57°/mm, whereas
/// the crystallographic reading of the same constants — and the measured rotatory power of real quartz —
/// is 18.6–18.8°/mm. Neither number is wrong; they answer different questions. `gyrationProjection` below
/// is the CRYSTALLOGRAPHIC formula, provided as the bridge to the literature, and is deliberately not used
/// to predict this engine.
module MuellerInverse =

    // -----------------------------------------------------------------------------------------------------
    // Elevated inputs: what a measurement IS, and what the unknowns ARE.
    // -----------------------------------------------------------------------------------------------------

    /// Which crystallographic axis the sample was cut normal to — i.e. how the crystal frame is carried
    /// into the lab frame before the sample azimuth is applied.
    ///
    /// `ZCut` leaves the crystal in its natural frame, so the crystal 3-axis lies along the surface
    /// normal; `XCut` and `YCut` carry the 3-axis into the surface plane about y and about x
    /// respectively; `TiltedCut` is the general intermediate cut about y. A two-case bool would have been
    /// wrong here — each further cut is a non-breaking addition to this DU and a breaking change to a
    /// flag, which is exactly what `YCut` demonstrates.
    ///
    /// WHY THREE PRINCIPAL CUTS AND NOT TWO. For a UNIAXIAL crystal the 1- and 2-axes are equivalent, so
    /// `ZCut` and `XCut` exhaust the distinct principal looks and `YCut` would be a duplicate of `XCut`.
    /// For a BIAXIAL crystal they are not: the three principal cuts put three DIFFERENT crystal axes
    /// along the surface normal and so see three different transverse planes — (1,2), (2,3) and (1,3) —
    /// each carrying its own pair of principal indices and its own pair of gyration diagonal components.
    /// It takes all three to see all three of each, which `BiaxialInverseTests` asserts by ablation.
    /// Azimuth cannot substitute, because rotating the sample about its own normal never changes WHICH
    /// crystal axis lies along that normal.
    type SampleCut =
        | ZCut
        | XCut
        | YCut
        | TiltedCut of Angle

    /// The sample's rotation about its own surface normal. Distinct from `Angle` in a signature so that a
    /// sample azimuth can never be passed where an incidence angle or an optic-axis tilt is expected.
    type SampleAzimuth =
        | SampleAzimuth of Angle

        /// The azimuth as the engine `Angle`.
        member this.angle = let (SampleAzimuth a) = this in a

        /// The azimuth in radians (the arithmetic seam).
        member this.value = let (SampleAzimuth a) = this in a.value

        /// Build a `SampleAzimuth` from a value given in degrees.
        static member degree (d : double) : SampleAzimuth = Angle.degree d |> SampleAzimuth

    /// Which Mueller matrix a configuration records. Transmission carries the sample's birefringence and
    /// optical activity over the whole optical path; reflection carries index information that never
    /// traverses the sample and is therefore independent of its thickness.
    type Observable =
        | TransmittedMueller
        | ReflectedMueller

    /// One measurement configuration — everything the forward model needs to predict a Mueller matrix,
    /// with nothing about the material in it. Every field is elevated: no bare degree, metre or nanometre
    /// appears anywhere in the record.
    type MeasurementConfiguration =
        {
            cut : SampleCut
            thickness : Thickness
            incidenceAngle : IncidenceAngle
            azimuth : SampleAzimuth
            observable : Observable
            waveLength : WaveLength
        }

    /// The name a fitted parameter is reported under — elevated so that a report cannot silently pair a
    /// name with the wrong column, and so that a name can never be passed where a reason or a description
    /// is expected.
    type ParameterName =
        | ParameterName of string

        /// The name as text (the reporting seam).
        member this.value = let (ParameterName n) = this in n

    /// ONE free parameter of an inverse problem: its identity, and how to read it out of and write it
    /// back into the material's own parameter record.
    ///
    /// This exists so that everything downstream of the material — the scaling, the residual, the
    /// Jacobian diagnostics, the acceptance report — is written ONCE and works for any material. A
    /// uniaxial gyrotropic crystal has four of these and a triclinic one has nine; nothing but the axis
    /// list changes between them.
    ///
    /// `read` and `write` speak plain `double` deliberately. The parameters of a single material carry
    /// DIFFERENT elevated types (`RefractionIndex` alongside `RhoValue`), so a double is the only thing
    /// they have in common, and this record is precisely the boundary at which the optimizer's
    /// dimensionless vector is allowed to see them.
    type ParameterAxis<'Parameters> =
        {
            name : ParameterName
            read : 'Parameters -> double
            write : double -> 'Parameters -> 'Parameters
        }

    /// The unknowns of the inverse problem for a UNIAXIAL gyrotropic crystal (classes 3/32/4/422/6/622):
    /// the two principal refractive indices and the two independent gyration-tensor components. Thickness
    /// and orientation are NOT here — they are known per configuration (spec R2) and belong to
    /// `MeasurementConfiguration`.
    type UniaxialParameters =
        {
            ordinaryIndex : RefractionIndex
            extraordinaryIndex : RefractionIndex
            g11 : RhoValue
            g33 : RhoValue
        }

        /// The four free parameters, in the fixed `[| n_o; n_e; g₁₁; g₃₃ |]` order that every scaled
        /// vector, Jacobian column and report in this module shares.
        static member axes : ParameterAxis<UniaxialParameters> list =
            [
                {
                    name = ParameterName "n_o"
                    read = fun p -> p.ordinaryIndex.value
                    write = fun v p -> { p with ordinaryIndex = RefractionIndex v }
                }
                {
                    name = ParameterName "n_e"
                    read = fun p -> p.extraordinaryIndex.value
                    write = fun v p -> { p with extraordinaryIndex = RefractionIndex v }
                }
                {
                    name = ParameterName "g11"
                    read = fun p -> p.g11.value
                    write = fun v p -> { p with g11 = RhoValue v }
                }
                {
                    name = ParameterName "g33"
                    read = fun p -> p.g33.value
                    write = fun v p -> { p with g33 = RhoValue v }
                }
            ]

    /// The unknowns of the inverse problem for a TRICLINIC (class 1) gyrotropic crystal — the most
    /// general transparent, non-magnetic, optically active crystal there is, and therefore the largest
    /// parameter set this machinery can be asked for.
    ///
    /// Class 1 has no symmetry element at all beyond the identity, so nothing constrains either tensor:
    /// ε contributes THREE independent principal refractive indices (the crystal is biaxial — every
    /// triclinic crystal is), and the gyration tensor, being a symmetric second-rank tensor, contributes
    /// all SIX of its independent components. Nine unknowns, against the uniaxial case's four. Every
    /// other optically active class is a constrained special case of this one: monoclinic 2 zeroes `g₁₂`
    /// and `g₂₃`, orthorhombic 222 zeroes all three off-diagonals, and the uniaxial classes additionally
    /// force `g₁₁ = g₂₂` and `n₁ = n₂`.
    ///
    /// The index order is NOT enforced here. Crystallographic convention labels the principal indices so
    /// that `n₁ ≤ n₂ ≤ n₃`, but an optimizer explores freely and a mid-search vector that violates it is
    /// a perfectly ordinary point of the residual surface, not an error — the forward model is total in
    /// all nine.
    type TriclinicParameters =
        {
            index1 : RefractionIndex
            index2 : RefractionIndex
            index3 : RefractionIndex
            g11 : RhoValue
            g22 : RhoValue
            g33 : RhoValue
            g23 : RhoValue
            g13 : RhoValue
            g12 : RhoValue
        }

        /// The nine free parameters, in the fixed order every scaled vector, Jacobian column and report
        /// for a triclinic sample shares: the three indices first, then the gyration diagonal, then the
        /// gyration off-diagonals.
        static member axes : ParameterAxis<TriclinicParameters> list =
            [
                {
                    name = ParameterName "n1"
                    read = fun p -> p.index1.value
                    write = fun v p -> { p with index1 = RefractionIndex v }
                }
                {
                    name = ParameterName "n2"
                    read = fun p -> p.index2.value
                    write = fun v p -> { p with index2 = RefractionIndex v }
                }
                {
                    name = ParameterName "n3"
                    read = fun p -> p.index3.value
                    write = fun v p -> { p with index3 = RefractionIndex v }
                }
                {
                    name = ParameterName "g11"
                    read = fun p -> p.g11.value
                    write = fun v p -> { p with g11 = RhoValue v }
                }
                {
                    name = ParameterName "g22"
                    read = fun p -> p.g22.value
                    write = fun v p -> { p with g22 = RhoValue v }
                }
                {
                    name = ParameterName "g33"
                    read = fun p -> p.g33.value
                    write = fun v p -> { p with g33 = RhoValue v }
                }
                {
                    name = ParameterName "g23"
                    read = fun p -> p.g23.value
                    write = fun v p -> { p with g23 = RhoValue v }
                }
                {
                    name = ParameterName "g13"
                    read = fun p -> p.g13.value
                    write = fun v p -> { p with g13 = RhoValue v }
                }
                {
                    name = ParameterName "g12"
                    read = fun p -> p.g12.value
                    write = fun v p -> { p with g12 = RhoValue v }
                }
            ]

    /// One measurement: a configuration paired with the Mueller matrix recorded in it.
    type MuellerObservation =
        {
            configuration : MeasurementConfiguration
            measured : MuellerMatrix
        }

    // -----------------------------------------------------------------------------------------------------
    // Forward-side helpers — pure geometry and tensor orientation.
    // -----------------------------------------------------------------------------------------------------

    /// The CRYSTALLOGRAPHIC gyration a wave propagating at internal angle `theta` to the optic axis sees:
    /// `G(θ) = g₁₁·sin²θ + g₃₃·cos²θ`, the double contraction of the gyration tensor with the propagation
    /// direction. At θ = 0 it collapses to `g₃₃` alone and at θ = 90° to `g₁₁` alone, so no single
    /// propagation direction constrains both — the structural reason more than one sample cut is needed.
    ///
    /// This is the LITERATURE convention and is provided as the bridge to published gyration values. It is
    /// deliberately NOT used to predict this engine's forward model, which parameterizes optical activity
    /// through the bi-anisotropic `ρ` of `D = εE + ρH` instead (see the module header): there, propagation
    /// along z is governed by the TRANSVERSE component `g₁₁` rather than by `g₃₃`, so the two conventions
    /// assign the roles of the two components oppositely.
    let gyrationProjection (theta : Angle) (g11 : RhoValue) (g33 : RhoValue) : RhoValue =
        let s = sin theta.value
        let c = cos theta.value
        g11.value * s * s + g33.value * c * c |> RhoValue

    /// The internal propagation angle for light entering a medium of index `n` from vacuum at
    /// `incidence` — Snell's law, `sin θ_int = sin θ_inc / n`. Its ceiling as `incidence → 90°` is
    /// `arcsin(1/n)`, the bound that makes `g₁₁` inaccessible from a z-cut sample however far it is tilted.
    let internalAngle (n : RefractionIndex) (incidence : IncidenceAngle) : Angle =
        asin (sin incidence.value / n.value) |> Angle

    /// Orient a crystal's optical properties for a given cut and sample azimuth.
    ///
    /// The material is built in its own crystal frame, where the principal axes of ε are the coordinate
    /// axes (that is what makes `ε₃₃ = n₃²`, and for a uniaxial crystal `g₃₃` the axial component). The
    /// lab frame stratifies along z, so:
    ///   • `ZCut` needs no rotation — crystal 3-axis and surface normal already coincide;
    ///   • `XCut` rotates the crystal 90° about y, carrying the 3-axis into the surface plane;
    ///   • `YCut` rotates it 90° about x, carrying the 3-axis into the surface plane the OTHER way, so
    ///     that a different crystal axis ends up along the normal;
    ///   • `TiltedCut a` rotates by `a` about y for the general intermediate cut.
    /// The sample azimuth is then a rotation about the surface normal (z), applied second. Every rotation
    /// goes through the ENGINE's own `OpticalProperties.rotate` (`MaterialProperties.fs:187`), which
    /// conjugates every tensor consistently — ε, μ and ρ together — so no tensor algebra is re-derived
    /// here and the gyration tensor can never fall out of step with the permittivity.
    let orientForCut (cut : SampleCut) (azimuth : SampleAzimuth) (properties : OpticalProperties) : OpticalProperties =
        let tilted =
            match cut with
            | ZCut -> properties
            | XCut -> properties.rotateY (Angle.degree 90.0)
            | YCut -> properties.rotateX (Angle.degree 90.0)
            | TiltedCut a -> properties.rotateY a
        tilted.rotateZ azimuth.angle

    // -----------------------------------------------------------------------------------------------------
    // Data reduction — normalization and the residual the fit minimizes.
    // -----------------------------------------------------------------------------------------------------

    /// A typed failure of the analytic inversion or of a Mueller normalization — never a throw.
    type InversionError =
        /// `m₀₀` is zero or negative, so the matrix carries no usable intensity to normalize by.
        | NonPositiveIntensity of m00 : float
        /// The polarization-subspace rotation cannot be resolved into elementary coefficients (a half-turn,
        /// where the rotation axis is ambiguous, or a block that is too far from a rotation to interpret).
        | DegenerateRotation of reason : string
        /// A model and a measurement list of different lengths were zipped into a residual.
        | MismatchedObservations of measured : int * model : int

    /// Divide a Mueller matrix through by `m₀₀`.
    ///
    /// Every comparison in this module is between NORMALIZED matrices, because absolute throughput is not a
    /// material property: it carries the source brightness, the detector gain and the Fresnel losses of the
    /// surfaces. Normalizing removes all of that and leaves the 15 numbers that describe how the sample
    /// TRANSFORMS polarization, which is what the material constants determine.
    let normalizeMueller (m : MuellerMatrix) : Result<MuellerMatrix, InversionError> =
        let m00 = Propagation.muellerElement m 0 0
        if m00 <= 0.0 || not (System.Double.IsFinite m00) then Error (NonPositiveIntensity m00)
        else
            Propagation.muellerOfRows
                [ for i in 0 .. 3 -> [ for j in 0 .. 3 -> Propagation.muellerElement m i j / m00 ] ]
            |> Ok

    /// The 15 elements of an ALREADY-normalized Mueller matrix other than `m₀₀`. `m₀₀` is excluded because
    /// normalization has forced it to exactly 1 in both model and measurement: including it would add a
    /// residual entry that is identically zero and silently deflate every reduced-χ² and standard error.
    let normalizedElements (m : MuellerMatrix) : float[] =
        [| for k in 1 .. 15 -> Propagation.muellerElement m (k / 4) (k % 4) |]

    /// The residual entries contributed by one observation: the element-wise difference between the
    /// normalized model and the normalized measurement, in a fixed element order.
    let observationResidual (measured : MuellerMatrix) (model : MuellerMatrix) : Result<float[], InversionError> =
        normalizeMueller measured
        |> Result.bind (fun nMeasured ->
            normalizeMueller model
            |> Result.map (fun nModel ->
                Array.map2 (-) (normalizedElements nModel) (normalizedElements nMeasured)))

    /// The full residual vector over every observation, in observation order. `models` must be the model
    /// prediction for each observation, in the same order — a length mismatch is a typed error rather than
    /// a truncated fit, because a silently short residual vector fits a different problem than the one
    /// asked for.
    let residualVector
        (observations : MuellerObservation list)
        (models : MuellerMatrix list)
        : Result<float[], InversionError> =
        if List.length observations <> List.length models then
            Error (MismatchedObservations (List.length observations, List.length models))
        else
            (Ok [], List.zip observations models)
            ||> List.fold (fun acc (o, model) ->
                acc |> Result.bind (fun parts ->
                    observationResidual o.measured model |> Result.map (fun r -> r :: parts)))
            |> Result.map (List.rev >> Array.concat)

    // -----------------------------------------------------------------------------------------------------
    // Stage A — the closed-form analytic inversion of a single Mueller matrix.
    //
    // For a homogeneous, non-depolarizing sample the normalized Mueller matrix resolves in closed form into
    // six elementary anisotropy coefficients: three birefringences and three dichroisms (Arteaga & Canillas,
    // Opt. Lett. 35, 559 (2010), with the erratum Opt. Lett. 35, 3525). For a TRANSPARENT sample the three
    // dichroisms are structurally zero, so the 3×3 polarization block is a pure rotation and the inversion
    // reduces to a rotation logarithm — and the recovered dichroisms become a free self-consistency check.
    //
    // Stage A produces STARTING VALUES and DIAGNOSTICS, never the reported answer. With multiple internal
    // reflections modelled (the plate solver sums the Mueller matrices of the emerging beams) the result is
    // weakly depolarizing by construction, so the Mueller–Jones assumption behind the inversion holds only
    // approximately. The authoritative stage is the nonlinear fit against the full Berreman forward model,
    // which carries the multiple reflections exactly and therefore has no such error.
    // -----------------------------------------------------------------------------------------------------

    /// A dichroism coefficient — a differential ATTENUATION between two orthogonal polarization states, as
    /// opposed to the differential PHASE that `Retardance` carries. Elevated separately precisely because
    /// the two are dimensionless in different ways and must never be interchanged at a call site.
    type Dichroism =
        | Dichroism of double

        /// The coefficient's value (the arithmetic seam).
        member this.value = let (Dichroism d) = this in d

    /// The six elementary anisotropy coefficients of a homogeneous sample.
    ///
    /// `lb` is the linear retardance with fast axis at 0°, `lbPrime` the linear retardance with fast axis
    /// at 45°, and `cb` the circular retardance (twice the azimuth rotation an optical rotator produces).
    /// `ld`, `ldPrime` and `cd` are the matching dichroisms, which a transparent sample must report as
    /// zero.
    type ElementaryAnisotropy =
        {
            lb : Retardance
            lbPrime : Retardance
            cb : Retardance
            ld : Dichroism
            ldPrime : Dichroism
            cd : Dichroism
        }

    /// The Mueller matrix of a pure birefringence — the forward direction of the analytic inversion, and
    /// the thing that PINS its sign conventions.
    ///
    /// The polarization block is the rotation `exp(−[v]×)` with `v = (lb, lbPrime, cb)`, embedded in a 4×4
    /// whose first row and column are `(1,0,0,0)`. The negative sign and the component order are not
    /// arbitrary: they are chosen so that this function reproduces the repository's OWN existing
    /// primitives, which the round-trip unit tests assert directly —
    ///   • `lb` alone reproduces `MuellerReconstruction.retarderMueller (Angle.zero) (Retardance lb)`;
    ///   • `cb` alone reproduces `Propagation.rotationMueller (Angle (cb / 2))`;
    ///   • `lbPrime` alone reproduces `retarderMueller (Angle.degree 45.0) (Retardance lbPrime)`.
    /// Deriving the convention from the code that already exists, rather than from a paper, is what keeps
    /// this consistent with every other Mueller matrix in the repository.
    let muellerOfBirefringence (lb : Retardance) (lbPrime : Retardance) (cb : Retardance) : MuellerMatrix =
        let v = [| -lb.value; -lbPrime.value; -cb.value |]
        let theta = sqrt (v.[0] * v.[0] + v.[1] * v.[1] + v.[2] * v.[2])
        // Rodrigues' formula, R = I + sinΘ·K + (1 − cosΘ)·K², with K the skew matrix of the unit axis.
        // At Θ = 0 the axis is undefined and the rotation is the identity, so short-circuit rather than
        // divide by zero.
        let block =
            if theta < 1.0e-300 then array2D [ [ 1.0; 0.0; 0.0 ]; [ 0.0; 1.0; 0.0 ]; [ 0.0; 0.0; 1.0 ] ]
            else
                let n = v |> Array.map (fun x -> x / theta)
                let k = array2D [ [ 0.0; -n.[2]; n.[1] ]; [ n.[2]; 0.0; -n.[0] ]; [ -n.[1]; n.[0]; 0.0 ] ]
                let k2 = Array2D.init 3 3 (fun i j -> [ 0 .. 2 ] |> List.sumBy (fun p -> k.[i, p] * k.[p, j]))
                Array2D.init 3 3 (fun i j ->
                    (if i = j then 1.0 else 0.0) + sin theta * k.[i, j] + (1.0 - cos theta) * k2.[i, j])
        Propagation.muellerOfRows
            [ for i in 0 .. 3 ->
                [ for j in 0 .. 3 ->
                    if i = 0 && j = 0 then 1.0
                    elif i = 0 || j = 0 then 0.0
                    else block.[i - 1, j - 1] ] ]

    /// Resolve one measured Mueller matrix into its six elementary anisotropy coefficients — the exact
    /// inverse of `muellerOfBirefringence` for a transparent sample.
    ///
    /// The dichroisms are read straight off the normalized first row, which IS the diattenuation vector:
    /// for a pure retarder it is `(0,0,0)`, so `transparencyResidual` measures directly how far the sample
    /// departs from the transparent, non-depolarizing ideal. The birefringences come from the rotation
    /// logarithm of the 3×3 polarization block: the trace gives the rotation angle and the antisymmetric
    /// part gives its axis.
    ///
    /// Two degeneracies are reported rather than papered over. A half-turn (Θ → π) leaves the antisymmetric
    /// part vanishing and the axis genuinely ambiguous in sign, and a block whose trace lies outside the
    /// range a rotation can produce is not a rotation at all. Both are `DegenerateRotation`: returning a
    /// plausible-looking number for either would corrupt the starting values silently.
    let analyticInversion (m : MuellerMatrix) : Result<ElementaryAnisotropy, InversionError> =
        normalizeMueller m
        |> Result.bind (fun n ->
            let e i j = Propagation.muellerElement n i j
            let trace = e 1 1 + e 2 2 + e 3 3
            let cosTheta = (trace - 1.0) / 2.0
            if cosTheta > 1.0 + 1.0e-6 || cosTheta < -1.0 - 1.0e-6 then
                Error (DegenerateRotation $"the polarization block has trace {trace}, which no rotation can produce")
            else
                let theta = acos (max -1.0 (min 1.0 cosTheta))
                let sinTheta = sin theta
                // Θ ≈ π is the ambiguous half-turn; Θ ≈ 0 is the identity, which is not ambiguous at all —
                // the rotation vector is simply zero, so it is handled rather than rejected.
                if theta > System.Math.PI - 1.0e-8 then
                    Error (DegenerateRotation $"the polarization rotation is a half-turn ({theta} rad), whose axis sign is ambiguous")
                else
                    let axisScale = if abs sinTheta < 1.0e-300 then 0.0 else theta / (2.0 * sinTheta)
                    // (B − Bᵀ)/2 = sinΘ·[n̂]×, and [n]× = [[0,−n₃,n₂],[n₃,0,−n₁],[−n₂,n₁,0]], so the axis
                    // components are read off the sub-diagonal entries. Multiplying by Θ/(2 sinΘ) converts
                    // the raw antisymmetric part straight into the rotation VECTOR Θ·n̂.
                    let v1 = axisScale * (e 3 2 - e 2 3)
                    let v2 = axisScale * (e 1 3 - e 3 1)
                    let v3 = axisScale * (e 2 1 - e 1 2)
                    // muellerOfBirefringence builds exp(−[v]×) from v = (lb, lbPrime, cb), so inverting the
                    // sign here is what makes the two functions exact inverses.
                    Ok
                        {
                            lb = Retardance -v1
                            lbPrime = Retardance -v2
                            cb = Retardance -v3
                            ld = Dichroism (e 0 1)
                            ldPrime = Dichroism (e 0 2)
                            cd = Dichroism (e 0 3)
                        })

    /// How far a sample departs from the transparent (dichroism-free) ideal: `|LD| + |LD′| + |CD|`. A
    /// perfectly transparent, non-depolarizing sample returns exactly 0, so this is a free correctness
    /// check on both the forward model and the inversion — if it is not small, one of the two is wrong.
    let transparencyResidual (a : ElementaryAnisotropy) : float =
        abs a.ld.value + abs a.ldPrime.value + abs a.cd.value

    /// The depolarization index `DI = √((Σᵢⱼ mᵢⱼ² − m₀₀²) / (3·m₀₀²))`, which is exactly 1 for a
    /// non-depolarizing (Mueller–Jones) matrix and falls toward 0 as depolarization grows.
    ///
    /// It is reported rather than assumed. Summing the emerging beams of a plate incoherently produces a
    /// weakly depolarizing matrix by construction, so `DI` is slightly BELOW 1 for any real plate and a
    /// test that demanded exactly 1 would be asserting something false.
    let depolarizationIndex (m : MuellerMatrix) : Result<float, InversionError> =
        let m00 = Propagation.muellerElement m 0 0
        if m00 <= 0.0 || not (System.Double.IsFinite m00) then Error (NonPositiveIntensity m00)
        else
            let total =
                [ for i in 0 .. 3 do
                    for j in 0 .. 3 -> Propagation.muellerElement m i j ** 2.0 ]
                |> List.sum
            sqrt ((total - m00 * m00) / (3.0 * m00 * m00)) |> Ok

    /// The four eigenvalues of the Cloude coherency matrix, sorted descending.
    ///
    /// The coherency matrix `H = ¼ Σ mᵢⱼ (σᵢ ⊗ σⱼ*)` is Hermitian, and a Mueller matrix describes a
    /// PHYSICALLY REALIZABLE process if and only if `H` is positive semi-definite. A negative eigenvalue
    /// therefore means the matrix could not have come from any real optical element — the sharpest
    /// available check that a forward model is producing physics rather than numbers. A non-depolarizing
    /// matrix has exactly one non-zero eigenvalue; the size of the others measures depolarization.
    ///
    /// `H` is assembled here straight from its DEFINITION as a sum of Pauli outer products rather than
    /// from transcribed element formulas. That costs a few lines and buys correctness by construction: the
    /// element-wise form of this matrix is notoriously easy to get subtly wrong (one conjugate or one sign
    /// out of place still yields a plausible-looking Hermitian matrix with plausible-looking eigenvalues),
    /// whereas the definition is checkable by eye. The unit tests pin it further — the identity Mueller
    /// matrix and every pure retarder must yield eigenvalues (1, 0, 0, 0).
    ///
    /// The eigen-decomposition goes through the backing MathNet matrix, reached exactly as
    /// `MuellerReconstruction.createMathNetSvd` reaches `.Svd()`: unwrap the `ComplexMatrix` wrapper and
    /// call the member the DU does not surface. Because `H` is Hermitian its eigenvalues are real up to
    /// round-off, so the imaginary parts are dropped.
    let cloudeEigenvalues (m : MuellerMatrix) : float list =
        // The Pauli set σ₀ = I, σ₁ = diag(1,−1), σ₂ = antidiag(1,1), σ₃ = [[0,−i],[i,0]].
        let i1 = Complex(0.0, 1.0)
        let z = Complex.Zero
        let one = Complex.One
        let pauli =
            [| array2D [ [ one; z ]; [ z; one ] ]
               array2D [ [ one; z ]; [ z; -one ] ]
               array2D [ [ z; one ]; [ one; z ] ]
               array2D [ [ z; -i1 ]; [ i1; z ] ] |]
        // H = ¼ Σᵢⱼ mᵢⱼ (σᵢ ⊗ σⱼ*), with the Kronecker index convention
        // (A ⊗ B)[2p+q, 2r+s] = A[p,r]·B[q,s].
        let h =
            [ for row in 0 .. 3 ->
                [ for col in 0 .. 3 ->
                    let p, q = row / 2, row % 2
                    let r, s = col / 2, col % 2
                    let mutable acc = Complex.Zero
                    for i in 0 .. 3 do
                        for j in 0 .. 3 do
                            acc <- acc + Complex(Propagation.muellerElement m i j, 0.0) * pauli.[i].[p, r] * Complex.Conjugate pauli.[j].[q, s]
                    acc * Complex(0.25, 0.0) ] ]
            |> ComplexMatrix.create
        let (ComplexMatrix backing) = h
        let values = backing.Evd().EigenValues
        [ for i in 0 .. 3 -> values.[i].Real ] |> List.sortDescending

    // -----------------------------------------------------------------------------------------------------
    // Stage B — parameter scaling, the forward seam, and the nonlinear-solve seam.
    // -----------------------------------------------------------------------------------------------------

    /// The map between the PHYSICAL parameters and the dimensionless, O(1) vector the optimizer actually
    /// searches over.
    ///
    /// This type exists because of a concrete numerical hazard, not for tidiness. The physical parameters
    /// span four orders of magnitude — refractive indices are ≈ 1.5 while gyration components are ≈ 1e-4 —
    /// and the ALGLIB Levenberg–Marquardt path differentiates with a FIXED ABSOLUTE step of 1e-6
    /// (`AlglibAdapter.runLm` calls `alglib.minlmcreatev(n, m, x0, 1.0e-6, &state)`). Against a gyration
    /// component of 1e-4 that step is a 1 % perturbation: far outside the linear regime the Jacobian
    /// assumes, so the search direction would be computed from a chord, not a derivative. Carrying each
    /// parameter as `(physical − centre) / scale` puts every component near unity, where the fixed step is
    /// a genuinely small perturbation of all four at once.
    ///
    /// Making it a TYPE rather than a convention is deliberate: a scaled vector and a physical parameter
    /// set are both "some numbers", and a fit that silently mixes them converges smoothly to the wrong
    /// answer instead of failing.
    ///
    /// It carries its material's `axes` rather than knowing any material's field names, which is what
    /// lets one scaling type, one `toScaled`, one `ofScaled` and everything built on them serve a
    /// four-parameter uniaxial fit and a nine-parameter triclinic one without a line of difference.
    type ParameterScaling<'Parameters> =
        {
            axes : ParameterAxis<'Parameters> list
            centre : 'Parameters
            scale : 'Parameters
        }

        /// How many free parameters the fit vector carries.
        member this.dimension : int = List.length this.axes

    /// Convert physical parameters to the dimensionless fit vector, in the material's own axis order.
    let toScaled (scaling : ParameterScaling<'Parameters>) (p : 'Parameters) : float[] =
        scaling.axes
        |> List.map (fun a -> (a.read p - a.read scaling.centre) / a.read scaling.scale)
        |> Array.ofList

    /// Convert the dimensionless fit vector back to physical parameters — the exact inverse of `toScaled`.
    let ofScaled (scaling : ParameterScaling<'Parameters>) (v : float[]) : 'Parameters =
        (scaling.centre, List.indexed scaling.axes)
        ||> List.fold (fun acc (i, a) -> a.write (a.read scaling.centre + v.[i] * a.read scaling.scale) acc)

    /// A typed failure of the forward model — never a throw across the proxy boundary.
    type ForwardModelError =
        | ForwardSolveFailed of configuration : MeasurementConfiguration * reason : string

    /// The FORWARD seam: predict the Mueller matrix a configuration would record for a given set of
    /// material parameters.
    ///
    /// It is a proxy for three reasons, in order of weight. First, `OpticalSystemSolver` THROWS on
    /// degenerate input — `getMuellerMatrix` ends in `failwith "Invalid combination of parameters in
    /// getMuellerMatrix!"` (`Solvers.fs:310`) — and the repository's rules require that exception to be
    /// caught at a boundary and mapped to a typed error, so that F# above the boundary never handles
    /// exceptions. Second, the fit's hot loop is entirely solver calls, so a memoizing or coarsened backend
    /// is a plausible later swap that must not touch the inverse logic. Third, a mock backend lets the
    /// residual, scaling and reporting logic be exercised with zero solver calls.
    [<ReferenceEquality>]
    type ForwardModelProxy<'Parameters> =
        {
            muellerOf : MeasurementConfiguration -> 'Parameters -> Result<MuellerMatrix, ForwardModelError>
        }

    /// Build the real Berreman-backed forward proxy.
    ///
    /// `buildProperties` is supplied by the caller and is what keeps this module material-agnostic: it maps
    /// the four unknowns onto a crystal of whatever symmetry class the caller is studying.
    /// `solverParameters` is passed explicitly rather than defaulted, because `numberOfReflections` changes
    /// the physics being modelled — it decides how many internal bounces of the plate are summed — and a
    /// value that important should be visible at the composition root instead of inherited silently.
    ///
    /// The sample is modelled as a `Plate` substrate between vacuum half-spaces: a free-standing crystal
    /// plate in air, which is what the measurement configurations describe.
    let createBerremanForward
        (buildProperties : 'Parameters -> OpticalProperties)
        (solverParameters : SolverParameters)
        : ForwardModelProxy<'Parameters> =
        {
            muellerOf =
                fun (configuration : MeasurementConfiguration) (parameters : 'Parameters) ->
                    try
                        let oriented = orientForCut configuration.cut configuration.azimuth (buildProperties parameters)
                        let system : OpticalSystem =
                            {
                                description = Some "spec 0044 inverse-problem sample plate"
                                upper = OpticalProperties.vacuum
                                films = []
                                substrate = { properties = oriented; thickness = configuration.thickness } |> Plate |> Some
                                lower = OpticalProperties.vacuum
                            }
                        let light = IncidentLightInfo.createInclined configuration.waveLength configuration.incidenceAngle
                        let solver = OpticalSystemSolver(light, system, solverParameters)
                        match configuration.observable with
                        | TransmittedMueller -> solver.muellerMatrixT () |> Ok
                        | ReflectedMueller -> solver.muellerMatrixR () |> Ok
                    with e -> Error (ForwardSolveFailed (configuration, e.Message))
        }

    /// Predict every observation's Mueller matrix, in observation order.
    let forwardModels
        (forward : ForwardModelProxy<'Parameters>)
        (parameters : 'Parameters)
        (observations : MuellerObservation list)
        : Result<MuellerMatrix list, ForwardModelError> =
        (Ok [], observations)
        ||> List.fold (fun acc o ->
            acc |> Result.bind (fun ms -> forward.muellerOf o.configuration parameters |> Result.map (fun m -> m :: ms)))
        |> Result.map List.rev

    /// One nonlinear least-squares call payload. `residual` is the closure the optimizer drives; the bounds
    /// are in the SCALED space, matching `initial`.
    type NonlinearRequest =
        {
            residual : float[] -> float[]
            initial : float[]
            lowerBounds : float[]
            upperBounds : float[]
            maxIterations : int
            epsX : float
        }

    /// The outcome of a nonlinear solve.
    type NonlinearSolution =
        {
            solution : float[]
            finalResiduals : float[]
            iterations : int
        }

    /// A typed failure of the nonlinear solve — never a throw across the proxy boundary.
    type NonlinearSolverError =
        | DidNotConverge of reason : string
        | InvalidNonlinearRequest of reason : string

    /// The nonlinear SOLVE seam. The optimizer is an external routine, so — exactly as spec 0042 decided
    /// for its linear solve — it sits behind a proxy whose real factory bakes in the backend. The real
    /// ALGLIB-backed factory lives in `OpticalConstructor.Optimization`, which is the only project allowed
    /// to see ALGLIB and which references this one; a mock satisfying this surface returns a canned
    /// solution with no optimizer involved at all.
    [<ReferenceEquality>]
    type NonlinearSolverProxy =
        {
            solveNonlinearLeastSquares : NonlinearRequest -> Result<NonlinearSolution, NonlinearSolverError>
        }

    // -----------------------------------------------------------------------------------------------------
    // Identifiability diagnostics — computed from the residual Jacobian.
    //
    // On noiseless synthetic data the reduced χ² is ~1e-30, and `FitQuality` computes
    // `Cov = reducedχ² · (JᵀJ)⁻¹`, so the standard errors and confidence intervals it reports collapse
    // toward zero and say nothing. The two quantities below survive that, because both are properties of
    // `J` alone (and the correlation matrix survives too — the reducedχ² factor cancels in
    // `corrᵢⱼ = covᵢⱼ / (σᵢσⱼ)`). They are therefore what an identifiability claim must rest on here.
    // -----------------------------------------------------------------------------------------------------

    /// The Euclidean norm of each column of the residual Jacobian — how much the residual moves when that
    /// one parameter moves. A near-zero column means the data does not constrain that parameter AT ALL:
    /// it is the direct, quantitative statement that a parameter is unobservable in a given measurement
    /// set.
    let jacobianColumnNorms (jacobian : float[][]) : float[] =
        if jacobian.Length = 0 then [||]
        else
            let n = jacobian.[0].Length
            [| for p in 0 .. n - 1 -> sqrt (jacobian |> Array.sumBy (fun row -> row.[p] * row.[p])) |]

    /// The condition number of the residual Jacobian — the ratio of its largest to its smallest singular
    /// value, computed through the engine's existing `RealMatrix` seam. A large value means some direction
    /// in parameter space barely moves the residual, so the fit along it is ill-conditioned; an infinite
    /// value means it does not move it at all. Returns `infinity` for a rank-deficient Jacobian rather
    /// than dividing by zero.
    let jacobianCondition (jacobian : float[][]) : float =
        if jacobian.Length = 0 then infinity
        else
            let (RealMatrix a) = RealMatrix.create jacobian
            let svd = a.Svd(false)
            let values = [ for i in 0 .. svd.S.Count - 1 -> svd.S.[i] ]
            match values with
            | [] -> infinity
            | _ ->
                let smallest = List.min values
                if smallest <= 0.0 then infinity else List.max values / smallest

    /// The parameter CORRELATION matrix, computed from the Jacobian alone:
    /// `corrᵢⱼ = Cᵢⱼ / √(Cᵢᵢ·Cⱼⱼ)` with `C = (JᵀJ)⁻¹`. A magnitude near 1 off the diagonal means those two
    /// parameters are only constrained in combination, so the fit determines a ridge rather than a point.
    ///
    /// Why this exists rather than reading `FitQuality.FitReport.correlation`. That report derives the
    /// correlation from `Cov = reducedχ²·(JᵀJ)⁻¹`, and in exact arithmetic the `reducedχ²` factor cancels
    /// in the ratio, leaving the correlation well defined however small the residual is. It does NOT
    /// cancel in floating point: on noiseless synthetic data the reduced χ² is ~1e-30, the covariance
    /// diagonal underflows, and the report's own zero-guard turns every off-diagonal correlation into a
    /// flat 0.0 — a value indistinguishable from "perfectly independent" and therefore actively
    /// misleading. Dropping the `reducedχ²` factor before the division, as here, keeps the quantity
    /// meaningful all the way down to a zero residual.
    ///
    /// `FitQuality` remains the right tool once real measurement noise is present; this is the diagnostic
    /// for the noiseless case.
    let jacobianCorrelation (jacobian : float[][]) : float[][] =
        if jacobian.Length = 0 then [||]
        else
            let n = jacobian.[0].Length
            let transposed = [| for p in 0 .. n - 1 -> [| for i in 0 .. jacobian.Length - 1 -> jacobian.[i].[p] |] |]
            let (RealMatrix j) = RealMatrix.create jacobian
            let (RealMatrix jt) = RealMatrix.create transposed
            let c = (RealMatrix (jt * j)).inverse
            let (RealMatrix cm) = c
            [| for i in 0 .. n - 1 ->
                [| for k in 0 .. n - 1 ->
                    let d = sqrt (cm.[i, i] * cm.[k, k])
                    if d > 0.0 then cm.[i, k] / d
                    elif i = k then 1.0
                    else 0.0 |] |]
