namespace OpticalConstructor.Storage

open System
open System.Globalization
open FSharp.Data
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.SourceSpec

/// Imported-spectrum sidecar parsing (§E.5). An `ImportedSpectrum` references a
/// tabulated λ→weight spectrum by relative sidecar path under the JSON-canonical
/// project; this module parses that sidecar with FSharp.Data and reduces every
/// wavelength to the engine meter base through the SOLE `Units` seam (§D.2). The
/// spectrum is NEVER embedded as an FsPickler `.binz` pickle of the project model
/// (binding rule 4). Following the `MaterialImport` precedent the parser reads the
/// file TEXT (`SpectrumSidecarText`), keeping filesystem IO and its exceptions out
/// of the parser; the error channel is `Result<_, SpectrumImportError>`.
module SpectralImport =

    /// Net-new import error channel (errors as values, §0).
    type SpectrumImportError =
        | MalformedSpectrumCsvErr of string
        | NoSpectrumDataErr of string

    /// The text of an imported spectrum's CSV sidecar (`ImportedSpectrum`'s `SpectrumSidecarPath`).
    type SpectrumSidecarText =
        | SpectrumSidecarText of string

        member this.value = let (SpectrumSidecarText t) = this in t

        /// Parse this tabulated `wavelength, weight` spectrum (§E.5). The wavelength
        /// column header declares the unit (nm/µm/Å), defaulting to nm; each λ is
        /// reduced to the canonical meter base via `Units.toMeters` and returned as a
        /// `WaveLength.Nm` whose `.value` is the canonical SI datum (meters). The
        /// returned weights are the raw tabulated values (re-normalised by the §E.8
        /// averaging layer).
        member this.parseSpectrum : Result<(WaveLength * IlluminationWeight) list, SpectrumImportError> =
            let inv = CultureInfo.InvariantCulture

            let tryFloat (s : string) : float option =
                match Double.TryParse(s.Trim(), NumberStyles.Float, inv) with
                | true, v -> Some v
                | _ -> None

            // A boundary unit from the wavelength-column header; nm (the common spectrum convention) by
            // default. Mirrors `NkCsvText.importMaterial`'s, whose default is µm.
            let unitFromHeader (header : string) : UnitOfMeasure =
                let h = header.ToLowerInvariant()
                if h.Contains "µm" || h.Contains "um" || h.Contains "mkm" || h.Contains "micro" then Micrometer
                elif h.Contains "ang" || h.Contains "å" then Angstrom
                else Nanometer

            try
                let csv = CsvFile.Parse(this.value)
                let unit =
                    match csv.Headers with
                    | Some h when h.Length > 0 -> unitFromHeader h.[0]
                    | _ -> Nanometer
                let rows =
                    csv.Rows
                    |> Seq.choose (fun r ->
                        let cols = r.Columns
                        if cols.Length >= 2 then
                            match tryFloat cols.[0], tryFloat cols.[1] with
                            | Some lam, Some weight ->
                                // Canonical meters via the sole Units seam, re-wrapped
                                // as a Nm WaveLength (its .value reduces back to meters).
                                let nm = (toMeters unit (UnitMagnitude lam)) / nmToMeter
                                Some (WaveLength.Nm nm, IlluminationWeight weight)
                            | _ -> None
                        else None)
                    |> List.ofSeq
                if List.isEmpty rows then Error (NoSpectrumDataErr "no spectrum rows found")
                else Ok rows
            with e -> Error (MalformedSpectrumCsvErr e.Message)
