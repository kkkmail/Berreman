namespace OpticalConstructor.Storage

open System
open System.Text
open System.Numerics
open System.Globalization
open FSharp.Data
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary

/// refractiveindex.info YAML + CSV import and CSV export (§D.9). Importers produce
/// in-memory `MaterialEntry` values (never FsPickler-pickled, §I.4); every parsed
/// wavelength is reduced to the engine meter base through the sole `Units` seam
/// (§D.2/§D.11) according to the file's declared unit. Tabulated entries interpolate
/// LINEARLY in λ so the closure is total over the chart range (no caching layer).
/// The error channel is `Result<_, ImportError>` — parsing never throws past the
/// boundary. The `string` argument and `exportCsv`'s result are the raw file TEXT,
/// keeping filesystem IO and its exceptions out of the parser.
module MaterialImport =

    /// Net-new import error channel (errors as values, §0 / §D.9).
    type ImportError =
        | MalformedYaml of string
        | MalformedCsv of string
        | NoData of string

    let private inv = CultureInfo.InvariantCulture

    let private tryFloat (s : string) : float option =
        match Double.TryParse(s.Trim(), NumberStyles.Float, inv) with
        | true, v -> Some v
        | _ -> None

    /// All floating-point tokens on a line (comma/space/tab separated).
    let private parseFloats (line : string) : float[] =
        line.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose tryFloat

    /// Pick a boundary unit from a wavelength-column header (refractiveindex.info
    /// tabulates µm; a CSV may declare nm). Defaults to µm.
    let private unitFromHeader (header : string) : UnitOfMeasure =
        let h = header.ToLowerInvariant()
        if h.Contains "nm" then Nanometer
        elif h.Contains "µm" || h.Contains "um" || h.Contains "mkm" || h.Contains "micro" then Micrometer
        elif h.Contains "ang" || h.Contains "å" then Angstrom
        else Micrometer

    /// Linear-in-λ interpolation over the tabulated samples, clamped to the ends so
    /// the closure is total. Samples are `(λ in meters, n, k)`, sorted by λ.
    let private buildTabulatedClosure (points : (float * float * float)[]) : WaveLength -> Eps =
        let sorted = points |> Array.sortBy (fun (l, _, _) -> l)
        fun w ->
            let lam = w.value / 1.0<meter>
            let n, k =
                if sorted.Length = 1 then
                    let (_, n, k) = sorted.[0] in n, k
                else
                    let (l0, n0, k0) = sorted.[0]
                    let (lN, nN, kN) = sorted.[sorted.Length - 1]
                    if lam <= l0 then n0, k0
                    elif lam >= lN then nN, kN
                    else
                        // Find the bracketing pair [i, i+1] and lerp.
                        let mutable i = 0
                        while i < sorted.Length - 2 && (let (l, _, _) = sorted.[i + 1] in l < lam) do
                            i <- i + 1
                        let (la, na, ka) = sorted.[i]
                        let (lb, nb, kb) = sorted.[i + 1]
                        let t = if lb = la then 0.0 else (lam - la) / (lb - la)
                        na + (nb - na) * t, ka + (kb - ka) * t
            createComplex n k |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex

    let private entryFromTabulated (id : string) (name : string) (category : MaterialCategory) (points : (float * float * float)[]) : MaterialEntry =
        {
            id = id
            name = name
            category = category
            description = Some "Imported tabulated n,k (refractiveindex.info)."
            properties = isotropicProperties (EpsWithDisp (buildTabulatedClosure points))
        }

    /// Collect tabulated `(λ in meters, n, k)` rows from data lines (those starting
    /// with a number), reducing λ from `unit` to the meter base via `Units.toMeters`.
    /// Two-column `tabulated n` rows map k = 0.
    let private tabulatedRows (unit : UnitOfMeasure) (lines : string[]) : (float * float * float)[] =
        lines
        |> Array.choose (fun line ->
            let t = line.Trim()
            if t.Length > 0 && (Char.IsDigit t.[0] || t.[0] = '-' || t.[0] = '.') then
                let f = parseFloats t
                if f.Length >= 2 then
                    let lamMeters = (toMeters unit f.[0]) / 1.0<meter>
                    Some (lamMeters, f.[1], (if f.Length >= 3 then f.[2] else 0.0))
                else None
            else None)

    /// Import a refractiveindex.info YAML page (§D.9). Supports the `tabulated n`/
    /// `tabulated nk` layout and the `formula 1` Sellmeier coefficient block; λ is
    /// taken in µm (the refractiveindex.info convention) and reduced to meters. A
    /// `formula 1` block builds the analytic closure
    /// `n² = 1 + c₀ + Σ Aᵢ·λ²/(λ² − Bᵢ²)` (λ in µm through the `Units` seam).
    let importRefractiveIndexInfo (yamlText : string) : Result<MaterialEntry, ImportError> =
        try
            let lines = yamlText.Replace("\r\n", "\n").Split('\n')
            let isFormula1 =
                lines |> Array.exists (fun l -> l.Contains "type:" && l.Contains "formula 1")
            if isFormula1 then
                match lines |> Array.tryFind (fun l -> l.TrimStart().StartsWith "coefficients:") with
                | Some cl ->
                    let nums = parseFloats (cl.Substring(cl.IndexOf(':') + 1))
                    if nums.Length < 1 then Error (NoData "formula 1: no coefficients")
                    else
                        let c0 = nums.[0]
                        let pairs = nums.[1..] |> Array.chunkBySize 2 |> Array.filter (fun p -> p.Length = 2)
                        let closure (w : WaveLength) : Eps =
                            let lam = fromMeters Micrometer w.value
                            let lam2 = lam * lam
                            let s = pairs |> Array.sumBy (fun p -> p.[0] * lam2 / (lam2 - p.[1] * p.[1]))
                            createComplex (sqrt (1.0 + c0 + s)) 0.0 |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex
                        Ok {
                            id = "imported-rii"
                            name = "Imported (refractiveindex.info formula 1)"
                            category = Glass
                            description = Some "Imported Sellmeier (refractiveindex.info formula 1)."
                            properties = isotropicProperties (EpsWithDisp closure)
                        }
                | None -> Error (MalformedYaml "formula 1 block has no coefficients line")
            else
                let rows = tabulatedRows Micrometer lines
                if rows.Length = 0 then Error (NoData "no tabulated rows found")
                else Ok (entryFromTabulated "imported-rii" "Imported (refractiveindex.info)" Semiconductor rows)
        with e -> Error (MalformedYaml e.Message)

    /// Import a refractiveindex.info-style n,k CSV (§D.9). Columns are
    /// `wavelength, n[, k]`; the wavelength column header declares the unit (nm/µm/Å),
    /// defaulting to µm. λ is reduced to the meter base via `Units.toMeters`.
    let importCsv (csvText : string) : Result<MaterialEntry, ImportError> =
        try
            let csv = CsvFile.Parse(csvText)
            let unit =
                match csv.Headers with
                | Some h when h.Length > 0 -> unitFromHeader h.[0]
                | _ -> Micrometer
            let rows =
                csv.Rows
                |> Seq.choose (fun r ->
                    let cols = r.Columns
                    if cols.Length >= 2 then
                        match tryFloat cols.[0], tryFloat cols.[1] with
                        | Some lam, Some n ->
                            let k = if cols.Length >= 3 then (tryFloat cols.[2] |> Option.defaultValue 0.0) else 0.0
                            Some ((toMeters unit lam) / 1.0<meter>, n, k)
                        | _ -> None
                    else None)
                |> Array.ofSeq
            if rows.Length = 0 then Error (NoData "no CSV data rows found")
            else Ok (entryFromTabulated "imported-csv" "Imported (CSV)" Glass rows)
        with e -> Error (MalformedCsv e.Message)

    /// Export n,k sampled over a `Range<WaveLength>` to CSV text (§D.9), reading the
    /// entry's dispersion through the engine `getEps` path. n is recovered as
    /// `√(ε₁₁)`. λ is written in nm via the `Units` seam (display only; the engine
    /// data stays in meters).
    let exportCsv (entry : MaterialEntry) (range : Range<WaveLength>) : string =
        let sb = StringBuilder()
        sb.AppendLine("wavelength_nm,n,k") |> ignore
        // Sample λ linearly in the canonical meter base over the range (the engine's
        // `getWaveLengthValue` re-wraps the meter magnitude as a Nm value, so we walk
        // meters directly and build the sample WaveLength via the engine `nmToMeter`).
        let s = range.startValue.value
        let e = range.endValue.value
        let n = max 1 range.numberOfPoints
        for i in 0 .. range.numberOfPoints do
            let m = s + (e - s) * (float i) / (float n)
            let w = WaveLength.Nm(m / nmToMeter)
            let nComplex = Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0, 0]
            let nm = wavelengthToUnit Nanometer w
            sb.AppendLine(String.Format(inv, "{0:G},{1:G},{2:G}", nm, nComplex.Real, nComplex.Imaginary)) |> ignore
        sb.ToString()
