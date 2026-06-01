/// §J.2 — repeat / period unit-cell builder [Standard] (010 Part II §1, the
/// DBR/Bragg/EUV-Mo–Si convenience). The user defines a unit cell as an ordered
/// list of reused engine `Layer` values (`Media.fs:24`) and a positive repeat
/// count; `expand` flattens it into an ordinary `List<Layer>` assignable straight
/// into `OpticalSystem.films` (`Media.fs:98`). The expanded films are ordinary
/// `Layer` records — indistinguishable from manually entered ones — and there is
/// NO new "super-layer"/"period" domain type; the project persists only the
/// expanded `films` (§A.7), never the unfactored period.
///
/// `expand` is deliberately the pure `List.replicate count cell |> List.concat`
/// with NO dependency on the slice-015 J.9 `Validation.fs`: the arc-runner runs
/// one round per slice, so a later-slice symbol is not yet compiled when slice
/// 014 builds. The `R < 1` rejection (AC-J9) is enforced by the slice-015 J.9
/// validator at the caller boundary and owned/tested in slice 015's
/// `ValidationTests.fs`, NOT here.
module OpticalConstructor.Ui.RepeatBuilder

open Berreman.Media

/// Expand a unit cell of `Layer`s repeated `count` times into a flat `List<Layer>`
/// in cell order (§J.2 / AC-J2). `count` copies of the `count`-length-`cell`
/// concatenated, so `expand cell count` has length `count * List.length cell` and
/// preserves cell order across every period. Pure: no IO, no validation seam.
let expand (cell : Layer list) (count : int) : Layer list =
    List.replicate count cell |> List.concat
