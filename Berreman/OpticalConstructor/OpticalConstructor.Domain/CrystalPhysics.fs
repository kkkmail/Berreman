namespace OpticalConstructor.Domain

open System.Numerics
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Active

/// The crystal-parameter records a material version can hold as its physics, each with its engine builder
/// (spec 0050 S11.1, .spec-md §3.16).
module CrystalPhysics =

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

        /// The engine's optical properties of a uniaxial gyrotropic crystal (classes 3/32/4/422/6/622) with
        /// these four constants, through the engine's class 3/4/6 builder (spec 0050 §3.16).
        member this.opticalProperties : OpticalProperties =
            OpticalProperties.type_3_4_6_Crystal
                (EpsValue.fromRefractionIndex this.ordinaryIndex)
                (EpsValue.fromRefractionIndex this.extraordinaryIndex)
                this.g11
                this.g33

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

        /// The engine's optical properties of a triclinic (class 1) gyrotropic crystal with these nine
        /// constants: its biaxial permittivity and full symmetric gyration tensor (spec 0050 §3.16).
        member this.opticalProperties : OpticalProperties =
            {
                eps = Eps.fromRefractionIndex (this.index1, this.index2, this.index3)
                mu = Mu.vacuum
                rho = Rho.type_1_Crystal this.g11 this.g22 this.g33 this.g23 this.g13 this.g12
            }

    /// The unknowns of the inverse problem for an ABSORBING, gyrotropic, TRICLINIC crystal — the most
    /// general transparent-or-absorbing, non-magnetic, optically active crystal there is, and therefore
    /// the largest parameter set this machinery can be asked for: FIFTEEN independent real numbers.
    ///
    /// THE COUNT, and why it is 15 rather than 9.
    ///
    ///   * ε becomes COMPLEX: `ε = ε′ + iε″`, a complex symmetric 3x3, which is 12 real numbers. The
    ///     crystal frame is DEFINED as the one in which ε′ is diagonal, which spends three of them on
    ///     the frame itself and leaves `n₁, n₂, n₃` — so ε′ contributes 3 and ε″ contributes all SIX of
    ///     its independent components. That is 9 for the permittivity alone.
    ///
    ///   * ε″ IS NOT DIAGONAL IN THE CRYSTAL FRAME, and that is the physical content of carrying six
    ///     components rather than three. Neumann's principle constrains ε′ and ε″ independently: in
    ///     orthorhombic and higher symmetry both are locked to the same crystallographic axes, in
    ///     monoclinic one axis is shared and the other two are free to rotate, and in TRICLINIC nothing
    ///     is shared at all. A triclinic absorbing crystal therefore has NO frame that diagonalizes both
    ///     — the observable consequence being the 'dispersion of the extinction directions' that low-
    ///     symmetry absorbing crystals are known for. Parameterizing ε″ by its six components in the ε′
    ///     frame captures that exactly, and does so without the gauge degeneracies and periodicity that
    ///     three Euler angles for a second principal frame would drag in.
    ///
    ///   * The GYRATION tensor contributes its usual SIX components. In principle it becomes complex
    ///     too — `g = g′ + ig″`, with `g′` driving circular birefringence and `g″` circular dichroism —
    ///     which would make it 12 real numbers. It does NOT here, and the reason is a property of this
    ///     engine rather than of the physics: the engine's ρ is the bi-anisotropic magnetoelectric
    ///     tensor, its IMAGINARY part drives optical rotation at first order, and its REAL part turns
    ///     out to have NO first-order effect at all — its influence is quadratic. At realistic gyration
    ///     magnitudes (~1e-5) that leaves a circular dichroism of ~1e-13, i.e. nothing. Six dead
    ///     parameters would make the fit singular, so they are not carried.
    ///     `AbsorbingBiaxialInverseTests` measures that quadratic scaling and asserts it, rather than
    ///     leaving it as a footnote.
    ///
    /// 3 + 6 + 6 = 15.
    ///
    /// WHAT IS NOT HERE, deliberately. The isotropic PHASE is unrecoverable from any intensity-based
    /// polarimetry — absolute phase is lost in forming a Mueller matrix from a Jones matrix — so it is
    /// not a parameter. The isotropic ABSORPTION is recoverable, but ONLY from absolute throughput: a
    /// normalized Mueller matrix carries 6 of the 8 elementary effects and the two it drops are exactly
    /// the isotropic phase and the isotropic absorption. `AbsorbingBiaxialInverseTests` asserts that
    /// consequence in both directions rather than assuming it.
    type AbsorbingTriclinicParameters =
        {
            /// The three principal refractive indices — the principal values of ε′, in the frame that
            /// diagonalizes it, which IS the crystal frame by definition.
            index1 : RefractionIndex
            index2 : RefractionIndex
            index3 : RefractionIndex
            /// The six independent components of ε″ IN THE ε′ FRAME. Off-diagonal entries here are the
            /// statement that absorption and refraction do not share principal axes.
            epsIm11 : EpsValue
            epsIm22 : EpsValue
            epsIm33 : EpsValue
            epsIm23 : EpsValue
            epsIm13 : EpsValue
            epsIm12 : EpsValue
            /// The six independent components of the gyration tensor's REAL part — circular
            /// birefringence, i.e. optical rotation.
            g11 : RhoValue
            g22 : RhoValue
            g33 : RhoValue
            g23 : RhoValue
            g13 : RhoValue
            g12 : RhoValue
        }

        /// The engine's optical properties of an absorbing triclinic gyrotropic crystal with these fifteen
        /// constants: ε″ is general in the ε′ frame and ρ stays purely imaginary (spec 0050 §3.16;
        /// specs/0044/.manual/018-absorbing-biaxial-inverse.md).
        member this.opticalProperties : OpticalProperties =
            let e (re : double) (im : double) = Complex(re, im)
            let n1Sq = this.index1.value * this.index1.value
            let n2Sq = this.index2.value * this.index2.value
            let n3Sq = this.index3.value * this.index3.value
            {
                eps =
                    [ [ e n1Sq this.epsIm11.value; e 0.0 this.epsIm12.value; e 0.0 this.epsIm13.value ]
                      [ e 0.0 this.epsIm12.value; e n2Sq this.epsIm22.value; e 0.0 this.epsIm23.value ]
                      [ e 0.0 this.epsIm13.value; e 0.0 this.epsIm23.value; e n3Sq this.epsIm33.value ] ]
                    |> Eps.create
                mu = Mu.vacuum
                rho =
                    [ [ e 0.0 this.g11.value; e 0.0 this.g12.value; e 0.0 this.g13.value ]
                      [ e 0.0 this.g12.value; e 0.0 this.g22.value; e 0.0 this.g23.value ]
                      [ e 0.0 this.g13.value; e 0.0 this.g23.value; e 0.0 this.g33.value ] ]
                    |> Rho.create
            }

    /// A crystal-parameter record a material version holds as its physics; stored in the serializer's case
    /// form, so it adds no int tag (spec 0050 S11.1).
    type CrystalParameters =
        | UniaxialCrystal of UniaxialParameters
        | TriclinicCrystal of TriclinicParameters
        | AbsorbingTriclinicCrystal of AbsorbingTriclinicParameters

        /// The engine's optical properties of the crystal, through its record's builder (spec 0050 S11.1).
        member this.opticalProperties : OpticalProperties =
            match this with
            | UniaxialCrystal parameters -> parameters.opticalProperties
            | TriclinicCrystal parameters -> parameters.opticalProperties
            | AbsorbingTriclinicCrystal parameters -> parameters.opticalProperties

        /// The crystal's engine composition: the same tensors at every wavelength (spec 0050 S11.1).
        member this.toProperties : OpticalPropertiesWithDisp = this.opticalProperties.dispersive
