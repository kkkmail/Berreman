/// Spec 0038 (032) — the Material editor's tabbed multi-curve dispersion preview builders, on the
/// shared dual-axis chart spine (spec 0033 018). Each builder walks the canonical-meter grid itself
/// and reads the engine tensors through the SAME `getEps` / `getRho` / `getMu` seams the
/// `MaterialImport.exportCsv` extraction uses — so a dispersive entry draws curves and a
/// non-dispersive one flat lines through ONE path, and a display unit relabels / rescales ONLY the
/// x-axis (AC-D7). Three charts feed the editor's tabs (and the Materials workbench View panel reuses
/// `nkDispersionChart`):
///   • `nkDispersionChart` — per PRINCIPAL AXIS: n₁/n₂/n₃ on the LEFT axis and k₁/k₂/k₃ on the RIGHT
///     (nᵢ = Re[√εᵢᵢ], kᵢ = Im[√εᵢᵢ] through the engine `epsWithDisp.getEps` path — no n/k re-derived).
///   • `gyrationChart` — the optical-activity gyration tensor components Im[ρᵢⱼ] vs wavelength,
///     assembled through the engine `RhoWithDispValue.toRhoWithDisp` path that BUILT `rhoWithDisp`
///     (`OpticalProperties/Active.fs`; `MaterialComplexity.toProperties`).
///   • `muChart` — the Polder μ components: the real diagonal μᵢᵢ on the LEFT axis and the gyration
///     magnitude on the RIGHT, through the `MuWithDispValue.toMuWithDisp` path that built `muWithDisp`
///     (`Berreman/Dispersion.fs`).
///
/// NOTE (§7 skepticism rule): the engine data builders `Analytics.Variables.calculateN11Re` /
/// `calculateXi11Im` sample wavelengths through `Analytics.getWaveLengthValue`, which re-wraps the
/// meter magnitude as a nm scalar and so evaluates the dispersion at λ×10⁻⁹ — a dispersive entry
/// would draw a FLAT line at the wrong value. Following the two in-repo precedents that document the
/// same defect (`SourceSpec.SpectralProfile.sample`, `MaterialImport.exportCsv`), these builders walk
/// the canonical meter grid themselves and read the tensors through the engine `getEps`/`getRho`/`getMu`
/// seams. Pure — no window, no ScottPlot.
module OpticalConstructor.Ui.NkDispersionChart

open System.Numerics
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ExperimentChart

/// The grid the builders share: the display-unit x positions and the index-aligned canonical
/// wavelengths (the `s + (e − s)·i/n` meter grid `SpectralAxis.axisTicks` walks — a display-unit x is
/// a pure rescale of the same grid position, never a re-sample, AC-D7).
let private grid (u : UnitOfMeasure) (range : Range<WaveLength>) : float list * WaveLength list =
    let xs = SpectralAxis.axisTicks u range
    let ws = SpectralAxis.axisTicks Nanometer range |> List.map (fun nm -> WaveLength.Nm (nm * 1.0<nm>))
    xs, ws

/// A single mid-band wavelength used to CLASSIFY a view-only preset's optional aspects (whether its
/// assembled engine tensors carry gyration / a non-vacuum μ) when there is no ladder toggle to read.
let private classifyWavelength : WaveLength = WaveLength.Nm 550.0<nm>

/// Flip the named series indices onto the RIGHT axis of the chart's default paired seed; every other
/// series keeps the LEFT axis (the 018 spine births every series on the left). The per-side auto
/// bounds then fit each axis independently (018 `dataBounds`).
let private rightAxisSeries (indices : int list) (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    indices
    |> List.fold (fun st i -> ChartStyle.setSeriesAxisSide i ChartStyle.RightAxis st) (ChartStyle.defaultState chart)

/// Spec 0038 comment 007: the principal-axis n/k slots a given anisotropy DISTINGUISHES, each a
/// (tensor diagonal index, n-series name, k-series name) triple — so the chart draws ONLY the curves
/// applicable to the choice, named the way the ladder names its axes. Isotropic distinguishes one axis
/// (a single n/k); uniaxial the ordinary vs the extraordinary; biaxial the three principal axes. The
/// extraordinary axis is the MIDDLE diagonal (index 1): the engine assembles a uniaxial ε as
/// `Eps.fromRefractionIndex (nO, nE, nO)` = diag(nₒ², nₑ², nₒ²) (`Dispersion.fs:297`).
let private nkAxisSpec (anisotropy : Anisotropy) : (int * string * string) list =
    match anisotropy with
    | Isotropic -> [ (0, "n", "k") ]
    | Uniaxial -> [ (0, "n_o", "k_o"); (1, "n_e", "k_e") ]
    | Biaxial -> [ (0, "n₁", "k₁"); (1, "n₂", "k₂"); (2, "n₃", "k₃") ]

/// The anisotropy to draw a VIEW-ONLY material entry with (the Materials window's preview panel, which
/// has no editor ladder to read): read from the entry's stored complexity value tree
/// (`LibraryFacets.anisotropyOf` — data, no physics re-derived) when it has one, else — a coded engine
/// preset whose physics is a closure, not data — fall back to all three principal axes (Biaxial), which
/// is safe: coincident axes simply draw coincident curves, as before.
let anisotropyOfEntry (entry : OpticalConstructor.Domain.MaterialLibrary.MaterialEntry) : Anisotropy =
    match entry.complexity with
    | Some complexity -> LibraryFacets.anisotropyOf complexity
    | None -> Biaxial

/// The per-axis n/k dispersion chart, restricted to the axes the anisotropy distinguishes
/// (spec 0038 comment 007): for each slot, nᵢ = Re[√εᵢᵢ] and kᵢ = Im[√εᵢᵢ] through the engine `getEps`
/// seam. The n series come first (LEFT axis), then the k series (RIGHT). An isotropic medium draws one
/// n and one k curve; a uniaxial one an ordinary and an extraordinary pair; a biaxial one three pairs.
let nkDispersionChart (anisotropy : Anisotropy) (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let xs, ws = grid u range
    // √εᵢᵢ sampled once per distinguished axis over the grid (exportCsv's n,k extraction, per axis).
    let sqrtDiag (i : int) : Complex list =
        ws |> List.map (fun w -> Complex.Sqrt ((o.epsWithDisp.getEps w).[i, i]))
    let sampled = nkAxisSpec anisotropy |> List.map (fun (i, nName, kName) -> nName, kName, sqrtDiag i)
    let nSeries = sampled |> List.map (fun (nName, _, d) -> { name = nName; points = List.zip xs (d |> List.map (fun c -> c.Real)) })
    let kSeries = sampled |> List.map (fun (_, kName, d) -> { name = kName; points = List.zip xs (d |> List.map (fun c -> c.Imaginary)) })
    {
        series = nSeries @ kSeries
        xLabel = SpectralAxis.axisLabel u
        yLabel = "n"
        title = "n / k dispersion"
        description = "n on the left axis, k on the right, per principal axis the anisotropy distinguishes (nᵢ = Re[√εᵢᵢ], kᵢ = Im[√εᵢᵢ]); a non-dispersive material draws flat lines."
        angular = false
    }

/// The n/k chart's paired style seed for a given anisotropy: the n series (the first half — one per
/// distinguished axis) keep the LEFT axis; the k series (the second half) are flipped to the RIGHT.
let nkDispersionStyle (anisotropy : Anisotropy) (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    let n = List.length (nkAxisSpec anisotropy)
    rightAxisSeries [ n .. 2 * n - 1 ] chart

/// The (i, j) tensor slot a gyration component denotes — the diagonal g₁₁/g₂₂/g₃₃ at
/// (0,0)/(1,1)/(2,2) and the upper-triangle g₁₂/g₁₃/g₂₃ at (0,1)/(0,2)/(1,2) — matching the
/// `GyrationComponent` cases (`MaterialComplexityEditor.fs:88-94`) the editor and the value tree name.
let private gyrationSlot (comp : GyrationComponent) : int * int =
    match comp with
    | G11 -> 0, 0
    | G22 -> 1, 1
    | G33 -> 2, 2
    | G12 -> 0, 1
    | G13 -> 0, 2
    | G23 -> 1, 2

/// Every gyration component in the six-slot display order — the fallback set drawn for a coded
/// engine preset with NO stored gyration class to read (preserving the pre-restriction rendering).
let allGyrationComponents : GyrationComponent list = [ G11; G22; G33; G12; G13; G23 ]

/// The gyration components a VIEW-ONLY entry's stored optical-activity value tree admits: the ones
/// its `active` gyration CLASS carries (`gyrationComponents`, generic over the constant `RhoValue`
/// vs the dispersive `DispersionFormula` payload) — data, no physics re-derived, mirroring
/// `anisotropyOfEntry`. A tree with no activity falls back to every component.
let gyrationComponentsOfComplexity (complexity : MaterialComplexity) : GyrationComponent list =
    match complexity.active with
    | Some (RhoWithoutDispValue g) -> gyrationComponents g.gyration |> List.map fst
    | Some (RhoWithDispValue g) -> gyrationComponents g.gyration |> List.map fst
    | None -> allGyrationComponents

/// The gyration chart (shown only when the entry is optically active), restricted to the components
/// the entry's symmetry class admits (spec 0040 step 006): ONE series per component in `components`,
/// named by `gyrationComponentLabel` and read as g₍ᵢⱼ₎ = Im[ρᵢⱼ] at the `gyrationSlot` (i,j). ρ is
/// sampled through the engine `rhoWithDisp.getRho` seam that `RhoWithDispValue.toRhoWithDisp` built.
/// All components share ONE (left) axis — they are the same physical scale (~10⁻⁵). A uniaxial-active
/// entry draws only g₁₁ and g₃₃; a coded preset with no stored class draws all six.
let gyrationChart (components : GyrationComponent list) (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let xs, ws = grid u range
    let rhos = ws |> List.map (fun w -> o.rhoWithDisp.getRho w)
    let series (comp : GyrationComponent) : ChartSeries =
        let i, j = gyrationSlot comp
        { name = gyrationComponentLabel comp; points = List.zip xs (rhos |> List.map (fun r -> r.[i, j].Imaginary)) }
    {
        series = components |> List.map series
        xLabel = SpectralAxis.axisLabel u
        yLabel = "g"
        title = "Gyration components"
        description = "Optical-activity gyration tensor components g₍ᵢⱼ₎ = Im[ρᵢⱼ] vs wavelength, per the entry's symmetry class."
        angular = false
    }

/// The gyration chart's style seed: every g component on the LEFT axis (a single shared scale).
let gyrationStyle (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    ChartStyle.defaultState chart

/// The series name of the Polder gyration magnitude (the one μ series flipped to the RIGHT axis) —
/// ONE constant shared by `muChart` (which emits it for a gyromagnetic tensor) and `muStyle` (which
/// sides it), so a scalar-μ chart that emits no such series simply keeps every series on the left.
let private muGyrationSeriesName : string = "g"

/// The Polder-μ kind a VIEW-ONLY entry's stored magnetic value tree denotes: a scalar μ vs the
/// gyromagnetic tensor (mirroring `anisotropyOfEntry`; data, no physics re-derived). The dispersive
/// Polder case is always the full tensor — the engine's `MuWithDispValue` carries no scalar
/// dispersive case. A tree with no magnetic facet falls back to the full gyromagnetic rendering.
let muKindOfComplexity (complexity : MaterialComplexity) : MuKind =
    match complexity.magnetic with
    | Some (MuWithoutDispValue (ScalarMu _)) -> ScalarMuKind
    | Some (MuWithoutDispValue (GyromagneticMu _)) -> GyromagneticMuKind
    | Some (MuWithDispValue _) -> GyromagneticMuKind
    | None -> GyromagneticMuKind

/// The Polder-μ chart (shown only when the entry is magnetic), restricted to the components the
/// entry's `MuKind` distinguishes (spec 0040 step 006): a SINGLE scalar μ series for `ScalarMuKind`
/// (a scalar permeability is μ·I — one curve says everything), versus the real diagonal μ₁₁/μ₂₂/μ₃₃
/// (LEFT axis) plus the gyration magnitude (RIGHT) for `GyromagneticMuKind`. μ is sampled through the
/// engine `muWithDisp.getMu` seam that `MuWithDispValue.toMuWithDisp` built; the gyration magnitude is
/// √(Σ_{i<j} Im[μᵢⱼ]²) — axis-invariant, since only the off-diagonal pair the magnetization axis
/// selects is non-zero in a Polder tensor.
let muChart (muKind : MuKind) (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let xs, ws = grid u range
    let mus = ws |> List.map (fun w -> o.muWithDisp.getMu w)
    let diagSeries (name : string) (i : int) : ChartSeries =
        { name = name; points = List.zip xs (mus |> List.map (fun m -> m.[i, i].Real)) }
    let gyrationMag (m : Mu) : float =
        let im (i : int) (j : int) : float = m.[i, j].Imaginary
        sqrt (im 0 1 * im 0 1 + im 0 2 * im 0 2 + im 1 2 * im 1 2)
    let series =
        match muKind with
        | ScalarMuKind -> [ diagSeries "μ" 0 ]
        | GyromagneticMuKind ->
            [
                diagSeries "μ₁₁" 0
                diagSeries "μ₂₂" 1
                diagSeries "μ₃₃" 2
                { name = muGyrationSeriesName; points = List.zip xs (mus |> List.map gyrationMag) }
            ]
    {
        series = series
        xLabel = SpectralAxis.axisLabel u
        yLabel = "μ"
        title = "Polder μ components"
        description = "Polder permeability diagonal μᵢᵢ (left axis) and the gyration magnitude (right axis) vs wavelength; a scalar μ draws its single component."
        angular = false
    }

/// The Polder-μ chart's style seed: every diagonal μ component on the LEFT axis, the gyration
/// magnitude series (when present) flipped to the RIGHT. A scalar-μ chart carries no gyration series,
/// so its single component stays on the left.
let muStyle (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    let gyrationIndices =
        chart.series
        |> List.mapi (fun i s -> i, s.name)
        |> List.choose (fun (i, name) -> if name = muGyrationSeriesName then Some i else None)
    rightAxisSeries gyrationIndices chart

/// Whether the entry's ASSEMBLED engine ρ carries any gyration at the classify wavelength — used to
/// decide the Gyration tab for a VIEW-ONLY preset, which has no activity toggle to read.
let hasGyration (o : OpticalPropertiesWithDisp) : bool =
    let r = o.rhoWithDisp.getRho classifyWavelength
    [ for i in 0 .. 2 do for j in 0 .. 2 -> r.[i, j].Magnitude ] |> List.exists (fun v -> v > 1e-12)

/// Whether the entry's ASSEMBLED engine μ deviates from the vacuum identity at the classify
/// wavelength — used to decide the μ tab for a VIEW-ONLY preset, which has no magnetic toggle to read.
let hasMagnetic (o : OpticalPropertiesWithDisp) : bool =
    let m = o.muWithDisp.getMu classifyWavelength
    let ident (i : int) (j : int) : Complex = if i = j then Complex.One else Complex.Zero
    [ for i in 0 .. 2 do for j in 0 .. 2 -> (m.[i, j] - ident i j).Magnitude ] |> List.exists (fun v -> v > 1e-12)
