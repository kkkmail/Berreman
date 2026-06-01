/// §J.9 — input validation & physical-sanity warnings [Standard] (010 Part II §8).
/// Pure boundary validators returning `Result<'T, ValidationError list>` (§0
/// error-handling rule — `Result`, not exceptions) for the editable boundary
/// fields. They run at the UI boundary BEFORE a value is committed to the model and
/// surface failures inline; they NEVER throw and NEVER silently coerce out-of-range
/// input. Validators operate on ALREADY-PARSED canonical values (e.g. a
/// `Thickness` carrying `double<meter>`, a `double<meter>` wavelength) — they do
/// NOT duplicate the §A.3 / `Units` unit-conversion seam.
///
/// This is the J.9 seam slice 014's J.2 repeat builder defers to for its `R < 1`
/// rejection (AC-J9): `RepeatBuilder.expand` carries no reference to this module
/// (one-round-per-slice), and the rejection is owned and tested here.
module OpticalConstructor.Ui.Validation

open System
open Berreman.Constants
open Berreman.Media

/// Whether a validation finding blocks commit (`Blocking`) or is an advisory
/// physical-sanity note (`Warning`, 010 Part II §8). Named to avoid shadowing the
/// `Result` `Error` constructor used throughout this module.
type Severity =
    | Blocking
    | Warning

/// A single validation finding surfaced inline next to the offending field (§J.9).
type ValidationError =
    {
        field : string
        severity : Severity
        message : string
    }

let private blocking field message = { field = field; severity = Blocking; message = message }
let private advisory field message = { field = field; severity = Warning; message = message }

/// A finite positive canonical length (meters): finite (not NaN/±∞) and `> 0`.
let private isFinitePositive (x : float<meter>) : bool =
    Double.IsFinite(float x) && x > 0.0<meter>

/// Validate a layer thickness (§J.9, `Media.fs:12`). A finite film MUST have a
/// strictly positive thickness — `Thickness.Thickness d` with `d <= 0.0<meter>`
/// (or a non-finite `d`) is rejected; the semi-infinite `Thickness.Infinity`
/// half-space is valid. Returns the validated `Thickness` unchanged on `Ok` (no
/// coercion).
let validateThickness (t : Thickness) : Result<Thickness, ValidationError list> =
    match t with
    | Infinity -> Ok t
    | Thickness d ->
        if isFinitePositive d then Ok t
        else Error [ blocking "thickness" "Film thickness must be a positive, finite length (meters > 0)." ]

/// Validate a repeat/period count (§J.2 / §J.9). A unit cell MUST repeat at least
/// once — a count `< 1` is rejected. This is the rejection slice 014's J.2 repeat
/// builder defers to (AC-J9).
let validateRepeatCount (count : int) : Result<int, ValidationError list> =
    if count >= 1 then Ok count
    else Error [ blocking "repeatCount" "Repeat count must be at least 1." ]

/// Validate a wavelength range (§J.9). Both endpoints MUST be finite and strictly
/// positive (canonical meters), and the range MUST be non-empty (`min < max`).
/// Accumulates every violation so all are surfaced inline at once. Returns the
/// validated `(min, max)` unchanged on `Ok` (no coercion, no reordering).
let validateWavelengthRange
    (minM : float<meter>)
    (maxM : float<meter>)
    : Result<float<meter> * float<meter>, ValidationError list> =
    let errors =
        [
            if not (isFinitePositive minM) then
                blocking "wavelengthMin" "Wavelength range minimum must be a positive, finite length."
            if not (isFinitePositive maxM) then
                blocking "wavelengthMax" "Wavelength range maximum must be a positive, finite length."
            if isFinitePositive minM && isFinitePositive maxM && minM >= maxM then
                blocking "wavelengthRange" "Wavelength range must be non-empty (min < max)."
        ]
    match errors with
    | [] -> Ok(minM, maxM)
    | _ -> Error errors

// ---------------------------------------------------------------------------
// Physical-sanity warnings (§J.9 / 010 Part II §8). Advisory, non-blocking notes —
// they flag suspect physics without rejecting the value. `k` is the already-parsed
// imaginary refractive index (dimensionless), supplied after the §A.3 boundary
// conversion; this seam runs no unit conversion of its own.
// ---------------------------------------------------------------------------

/// Physical-sanity warning (010 Part II §8): a finite, negative imaginary
/// refractive index implies optical GAIN (an amplifying medium) rather than
/// absorption. Returns a non-blocking `Warning` (it does not gate commit); an
/// absorbing (`k >= 0`) or non-finite `k` yields no warning.
let imaginaryIndexGainWarning (k : float) : ValidationError list =
    if Double.IsFinite k && k < 0.0 then
        [ advisory "refractiveIndexK" "Imaginary refractive index k < 0 implies gain (an amplifying medium), not absorption." ]
    else []
