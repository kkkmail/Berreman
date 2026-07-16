namespace OpticalConstructor.Domain

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields

/// Spec 0042 (001) — the pure Mueller-reconstruction primitives. A *retarder* (wave plate) delays one
/// linear eigen-polarization relative to the other by a phase `Retardance`; its Mueller matrix is the
/// diagonal retarder core (fast axis at 0°) physically rotated to the plate's azimuth. It is built ONLY
/// from the EXISTING `Propagation` seams (`muellerOfRows` + `rotateMueller`) so no new 4×4 algebra is
/// introduced — the rotation reuses the same R(−θ)·M·R(θ) convention the ideal-polarizer pipeline pins.
module MuellerReconstruction =

    /// The phase delay a retarder (wave plate) introduces between its two linear eigen-polarizations,
    /// elevated to its own single-case DU. It is a PHASE, deliberately distinct from the azimuth `Angle`
    /// (`Geometry.fs`): mixing the two up is exactly the bug this elevation prevents. Mirrors the engine's
    /// `Angle` shape — a `.value` in radians, a `.degrees` view, and a `degree`-taking constructor.
    type Retardance =
        | Retardance of double

        /// The retardance in radians (the IO seam — read only where a raw phase is needed).
        member this.value = let (Retardance r) = this in r

        /// The retardance expressed in degrees.
        member this.degrees = let (Retardance r) = this in (r / degree)

        /// Build a `Retardance` from a value given in degrees.
        static member degree (d : double) : Retardance = d * degree |> Retardance

    /// The Mueller matrix of an ideal (lossless) retarder whose fast axis is at azimuth `axis` and whose
    /// phase delay is `retardance`. The diagonal retarder core (fast axis at 0°) is
    ///   [1,0,0,0], [0,1,0,0], [0,0,cos d,sin d], [0,0,−sin d,cos d]   (d = retardance in radians),
    /// physically rotated to `axis` by the EXISTING `Propagation.rotateMueller` (R(−θ)·M·R(θ)). With
    /// `Retardance.degree 0.0` the core is the identity and the result is `Propagation.identityMueller`
    /// for any azimuth; with a quarter wave (d = 90°) at azimuth 0° it is the standard QWP form.
    let retarderMueller (axis : Angle) (retardance : Retardance) : MuellerMatrix =
        let d = retardance.value
        let c = cos d
        let s = sin d
        let core =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; c; s ]
                  [ 0.0; 0.0; -s; c ] ]
        Propagation.rotateMueller axis core
