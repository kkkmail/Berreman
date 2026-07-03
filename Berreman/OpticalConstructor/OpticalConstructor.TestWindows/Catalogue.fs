/// The optical-element catalogue names / codes (Spec 0027). Pulled out of `TableAndElementRotationView`
/// into its own early module (task 018) so BOTH the shared element renderer and the scenes can use them
/// without a compile-order cycle. `TableAndElementRotationView` re-exports these for back-compat.
module OpticalConstructor.TestWindows.Catalogue

open OpticalConstructor.Domain.Placement

/// The full display name of a catalogue kind.
let kindName (k : CatalogueKind) : string =
    match k with
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
let kindCode (k : CatalogueKind) : string =
    match k with
    | LightSource -> "S"
    | LinearPolarizer -> "LP"
    | CircularPolarizer -> "CP"
    | Sample -> "Sa"
    | Lens -> "L"
    | FlatMirror -> "FM"
    | CurvedMirror -> "CM"
    | Detector -> "D"

/// A schematic optical sign for the shape renderer's lens / curved-mirror spherical caps (a bare
/// placement carries no signed radius of curvature): +1 = converging (convex lens / concave mirror) for
/// lenses and curved mirrors, 0 = flat (everything else, drawn as a plain cylinder).
let opticalSign (k : CatalogueKind) : int =
    match k with
    | Lens | CurvedMirror -> 1
    | _ -> 0
