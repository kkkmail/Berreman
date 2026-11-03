/// The optical-element catalogue names / codes (Spec 0027). Pulled out of `TableAndElementRotationView`
/// into its own early module (task 018) so BOTH the shared element renderer and the scenes can use them
/// without a compile-order cycle; members of `CatalogueKind` since spec 0048 E028 (ELEV013).
[<AutoOpen>]
module OpticalConstructor.Ui.Catalogue

open OpticalConstructor.Domain.Placement

/// A schematic optical sign for the shape renderer's lens / curved-mirror spherical caps (a bare placement
/// carries no signed radius of curvature). The cases sort diverging < flat < converging, as −1 < 0 < +1 did.
type OpticalSign =
    | DivergingSign
    | FlatSign
    | ConvergingSign

type CatalogueKind with

    /// The full display name of a catalogue kind.
    member this.kindName : string =
        match this with
        | LightSource -> "Light source"
        | LinearPolarizer -> "Linear polarizer"
        | CircularPolarizer -> "Circular polarizer"
        | Sample -> "Sample"
        | Lens -> "Lens"
        | FlatMirror -> "Flat mirror"
        | CurvedMirror -> "Curved mirror"
        | Detector -> "Detector"

    /// The short element code shown near an element and used as the palette button / add id:
    /// S = source, LP/CP = linear/circular polarizer, Sa = sample, L = lens, FM/CM = flat/curved mirror,
    /// D = detector.
    member this.kindCode : string =
        match this with
        | LightSource -> "S"
        | LinearPolarizer -> "LP"
        | CircularPolarizer -> "CP"
        | Sample -> "Sa"
        | Lens -> "L"
        | FlatMirror -> "FM"
        | CurvedMirror -> "CM"
        | Detector -> "D"

    /// The kind's optical sign: converging (convex lens / concave mirror) for lenses and curved mirrors,
    /// flat for everything else (drawn as a plain cylinder).
    member this.opticalSign : OpticalSign =
        match this with
        | Lens | CurvedMirror -> ConvergingSign
        | _ -> FlatSign
