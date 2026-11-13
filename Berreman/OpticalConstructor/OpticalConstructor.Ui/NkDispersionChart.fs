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
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ExperimentChart

/// The grid the builders share: the display-unit x positions and the index-aligned canonical
/// wavelengths (the `s + (e − s)·i/n` meter grid `SpectralAxis.axisTicks` walks — a display-unit x is
/// a pure rescale of the same grid position, never a re-sample, AC-D7).
let private grid (u : UnitOfMeasure) (range : Range<WaveLength>) : UnitMagnitude list * WaveLength list =
    let xs = SpectralAxis.axisTicks u range
    let ws = SpectralAxis.axisTicks Nanometer range |> List.map (fun nm -> WaveLength.Nm (nm.value * 1.0<nm>))
    xs, ws

/// A single mid-band wavelength used to CLASSIFY a view-only preset's optional aspects (whether its
/// assembled engine tensors carry gyration / a non-vacuum μ) when there is no ladder toggle to read.
let private classifyWavelength : WaveLength = WaveLength.Nm 550.0<nm>

/// Flip the named series onto the RIGHT axis of the chart's default paired seed; every other
/// series keeps the LEFT axis (the 018 spine births every series on the left). The per-side auto
/// bounds then fit each axis independently (018 `dataBounds`).
let private rightAxisSeries (indices : ChartStyle.ChartSeriesIndex list) (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    indices
    |> List.fold (fun st i -> ChartStyle.setSeriesAxisSide i ChartStyle.RightAxis st) (ChartStyle.defaultChartStyleState chart)

/// One principal axis's pair of n / k curves: the tensor diagonal it reads and the two series names.
type NkAxisCurves =
    {
        axis : PrincipalAxisSlot
        nName : ControlCaption
        kName : ControlCaption
    }

/// The (row, column) slot of a gyration component in the 3 × 3 ρ tensor.
type GyrationTensorSlot =
    {
        row : PrincipalAxisSlot
        column : PrincipalAxisSlot
    }

type PrincipalAxisSlot with

    /// The zero-based row / column of this principal axis in a 3 × 3 engine tensor.
    member this.tensorIndex : int =
        match this with
        | FirstAxis -> 0
        | SecondAxis -> 1
        | ThirdAxis -> 2

type Anisotropy with

    /// Spec 0038 comment 007: the principal-axis n/k curves this anisotropy DISTINGUISHES — so the chart
    /// draws ONLY the curves applicable to the choice, named the way the ladder names its axes. Isotropic
    /// distinguishes one axis (a single n/k); uniaxial the ordinary vs the extraordinary; biaxial the three
    /// principal axes. The extraordinary axis is the MIDDLE diagonal: the engine assembles a uniaxial ε as
    /// `Eps.fromRefractionIndex (nO, nE, nO)` = diag(nₒ², nₑ², nₒ²) (`Dispersion.fs:297`).
    member this.nkAxisCurves : NkAxisCurves list =
        match this with
        | Isotropic -> [ { axis = FirstAxis; nName = ControlCaption "n"; kName = ControlCaption "k" } ]
        | Uniaxial ->
            [ { axis = FirstAxis; nName = ControlCaption "n_o"; kName = ControlCaption "k_o" }
              { axis = SecondAxis; nName = ControlCaption "n_e"; kName = ControlCaption "k_e" } ]
        | Biaxial ->
            [ { axis = FirstAxis; nName = ControlCaption "n₁"; kName = ControlCaption "k₁" }
              { axis = SecondAxis; nName = ControlCaption "n₂"; kName = ControlCaption "k₂" }
              { axis = ThirdAxis; nName = ControlCaption "n₃"; kName = ControlCaption "k₃" } ]

/// The per-axis n/k dispersion chart, restricted to the axes the anisotropy distinguishes
/// (spec 0038 comment 007): for each axis, nᵢ = Re[√εᵢᵢ] and kᵢ = Im[√εᵢᵢ] through the engine `getEps`
/// seam. The n series come first (LEFT axis), then the k series (RIGHT). An isotropic medium draws one
/// n and one k curve; a uniaxial one an ordinary and an extraordinary pair; a biaxial one three pairs.
let nkDispersionChart (anisotropy : Anisotropy) (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let xs, ws = grid u range
    // √εᵢᵢ sampled once per distinguished axis over the grid (exportCsv's n,k extraction, per axis).
    let sqrtDiag (axis : PrincipalAxisSlot) : Complex list =
        let i = axis.tensorIndex
        ws |> List.map (fun w -> Complex.Sqrt ((o.epsWithDisp.getEps w).[i, i]))
    let sampled = anisotropy.nkAxisCurves |> List.map (fun curves -> curves, sqrtDiag curves.axis)
    let curve (name : ControlCaption) (ys : float list) : ExperimentControls.ChartSeries =
        { ExperimentControls.ChartSeries.name = name; points = List.map2 (fun (x : UnitMagnitude) y -> { ExperimentControls.ChartPoint.x = x.value; y = y }) xs ys }
    let nSeries = sampled |> List.map (fun (curves, d) -> curve curves.nName (d |> List.map (fun c -> c.Real)))
    let kSeries = sampled |> List.map (fun (curves, d) -> curve curves.kName (d |> List.map (fun c -> c.Imaginary)))
    {
        series = nSeries @ kSeries
        xLabel = ControlCaption u.axisLabel
        yLabel = ControlCaption "n"
        title = ControlCaption "n / k dispersion"
        description = ControlCaption "n on the left axis, k on the right, per principal axis the anisotropy distinguishes (nᵢ = Re[√εᵢᵢ], kᵢ = Im[√εᵢᵢ]); a non-dispersive material draws flat lines."
        xAxisKind = NonAngularXAxis
    }

/// The n/k chart's paired style seed for a given anisotropy: the n series (the first half — one per
/// distinguished axis) keep the LEFT axis; the k series (the second half) are flipped to the RIGHT.
let nkDispersionStyle (anisotropy : Anisotropy) (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    let n = List.length anisotropy.nkAxisCurves
    rightAxisSeries [ for i in n .. 2 * n - 1 -> ChartStyle.ChartSeriesIndex i ] chart

type GyrationComponent with

    /// The ρ tensor slot this component denotes — the diagonal g₁₁/g₂₂/g₃₃ and the upper-triangle
    /// g₁₂/g₁₃/g₂₃ — matching the cases the editor and the value tree name.
    member this.tensorSlot : GyrationTensorSlot =
        match this with
        | G11 -> { row = FirstAxis; column = FirstAxis }
        | G22 -> { row = SecondAxis; column = SecondAxis }
        | G33 -> { row = ThirdAxis; column = ThirdAxis }
        | G12 -> { row = FirstAxis; column = SecondAxis }
        | G13 -> { row = FirstAxis; column = ThirdAxis }
        | G23 -> { row = SecondAxis; column = ThirdAxis }

/// Every gyration component in the six-slot display order — the set drawn when a complexity names
/// no gyration class.
let allGyrationComponents : GyrationComponent list = [ G11; G22; G33; G12; G13; G23 ]

/// The gyration components a VIEW-ONLY entry's stored optical-activity value tree admits: the ones
/// its `active` gyration CLASS carries (`GyrationClass.components`, generic over the constant `RhoValue`
/// vs the dispersive `DispersionFormula` payload) — data, no physics re-derived. A tree with no
/// activity, or with a published gyration law (no stored class, spec 0050 step 10), names no
/// gyration class, so every component is drawn.
let gyrationComponentsOfComplexity (complexity : MaterialComplexity) : GyrationComponent list =
    match complexity.active with
    | Some (RhoWithoutDispValue g) -> g.gyration.components |> List.map fst
    | Some (RhoWithDispValue g) -> g.gyration.components |> List.map fst
    | Some (PublishedLawRho _)
    | None -> allGyrationComponents

/// The gyration chart (shown only when the entry is optically active), restricted to the components
/// the entry's symmetry class admits (spec 0040 step 006): ONE series per component in `components`,
/// named by `GyrationComponent.label` and read as g₍ᵢⱼ₎ = Im[ρᵢⱼ] at its `tensorSlot`. ρ is
/// sampled through the engine `rhoWithDisp.getRho` seam that `RhoWithDispValue.toRhoWithDisp` built.
/// All components share ONE (left) axis — they are the same physical scale (~10⁻⁵). A uniaxial-active
/// entry draws only g₁₁ and g₃₃; a complexity that names no gyration class draws all six.
let gyrationChart (components : GyrationComponent list) (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let xs, ws = grid u range
    let rhos = ws |> List.map (fun w -> o.rhoWithDisp.getRho w)
    let series (comp : GyrationComponent) : ExperimentControls.ChartSeries =
        let i = comp.tensorSlot.row.tensorIndex
        let j = comp.tensorSlot.column.tensorIndex
        {
            ExperimentControls.ChartSeries.name = ControlCaption comp.label
            points = List.map2 (fun (x : UnitMagnitude) (r : Rho) -> { ExperimentControls.ChartPoint.x = x.value; y = r.[i, j].Imaginary }) xs rhos
        }
    {
        series = components |> List.map series
        xLabel = ControlCaption u.axisLabel
        yLabel = ControlCaption "g"
        title = ControlCaption "Gyration components"
        description = ControlCaption "Optical-activity gyration tensor components g₍ᵢⱼ₎ = Im[ρᵢⱼ] vs wavelength, per the entry's symmetry class."
        xAxisKind = NonAngularXAxis
    }

type ExperimentChart with

    /// The gyration chart's style seed: every g component on the LEFT axis (a single shared scale).
    member this.gyrationStyle : ChartStyle.ChartStyleState =
        ChartStyle.defaultChartStyleState this

/// The series name of the Polder gyration magnitude (the one μ series flipped to the RIGHT axis) —
/// ONE constant shared by `muChart` (which emits it for a gyromagnetic tensor) and `muStyle` (which
/// sides it), so a scalar-μ chart that emits no such series simply keeps every series on the left.
let private muGyrationSeriesName : ControlCaption = ControlCaption "g"

type MaterialComplexity with

    /// The Polder-μ kind a VIEW-ONLY entry's stored magnetic value tree denotes: a scalar μ vs the
    /// gyromagnetic tensor (data, no physics re-derived). The
    /// dispersive Polder case is always the full tensor — the engine's `MuWithDispValue` carries no scalar
    /// dispersive case. A tree with no magnetic facet falls back to the full gyromagnetic rendering.
    member this.muKind : MuKind =
        match this.magnetic with
        | Some (MuWithoutDispValue (ScalarMu _)) -> ScalarMuKind
        | Some (MuWithoutDispValue (GyromagneticMu _)) -> GyromagneticMuKind
        | Some (MuWithDispValue _) -> GyromagneticMuKind
        | None -> GyromagneticMuKind

type MaterialPhysics with

    /// The gyration components a VIEW-ONLY entry's physics admits (spec 0050 S11.1).
    member this.gyrationComponents : GyrationComponent list =
        match this with
        | ComplexityPhysics complexity -> gyrationComponentsOfComplexity complexity

    /// The Polder-μ kind a VIEW-ONLY entry's physics denotes (spec 0050 S11.1).
    member this.muKind : MuKind =
        match this with
        | ComplexityPhysics complexity -> complexity.muKind

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
    let curve (name : ControlCaption) (ys : float list) : ExperimentControls.ChartSeries =
        { ExperimentControls.ChartSeries.name = name; points = List.map2 (fun (x : UnitMagnitude) y -> { ExperimentControls.ChartPoint.x = x.value; y = y }) xs ys }
    let diagSeries (name : string) (axis : PrincipalAxisSlot) : ExperimentControls.ChartSeries =
        let i = axis.tensorIndex
        curve (ControlCaption name) (mus |> List.map (fun m -> m.[i, i].Real))
    let gyrationMag (m : Mu) : float =
        let im (i : int) (j : int) : float = m.[i, j].Imaginary
        sqrt (im 0 1 * im 0 1 + im 0 2 * im 0 2 + im 1 2 * im 1 2)
    let series =
        match muKind with
        | ScalarMuKind -> [ diagSeries "μ" FirstAxis ]
        | GyromagneticMuKind ->
            [
                diagSeries "μ₁₁" FirstAxis
                diagSeries "μ₂₂" SecondAxis
                diagSeries "μ₃₃" ThirdAxis
                curve muGyrationSeriesName (mus |> List.map gyrationMag)
            ]
    {
        series = series
        xLabel = ControlCaption u.axisLabel
        yLabel = ControlCaption "μ"
        title = ControlCaption "Polder μ components"
        description = ControlCaption "Polder permeability diagonal μᵢᵢ (left axis) and the gyration magnitude (right axis) vs wavelength; a scalar μ draws its single component."
        xAxisKind = NonAngularXAxis
    }

/// The Polder-μ chart's style seed: every diagonal μ component on the LEFT axis, the gyration
/// magnitude series (when present) flipped to the RIGHT. A scalar-μ chart carries no gyration series,
/// so its single component stays on the left.
let muStyle (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    let gyrationIndices =
        chart.series
        |> List.mapi (fun i s -> ChartStyle.ChartSeriesIndex i, s.name)
        |> List.choose (fun (i, name) -> if name = muGyrationSeriesName then Some i else None)
    rightAxisSeries gyrationIndices chart

/// Whether the entry's ASSEMBLED engine ρ carries any gyration at the classify wavelength — used to
/// decide the Gyration tab for a VIEW-ONLY preset, which has no activity toggle to read.
let assembledActivity (o : OpticalPropertiesWithDisp) : ActivityChoice =
    let r = o.rhoWithDisp.getRho classifyWavelength
    match [ for i in 0 .. 2 do for j in 0 .. 2 -> r.[i, j].Magnitude ] |> List.exists (fun v -> v > 1e-12) with
    | true -> ActivityOn
    | false -> ActivityOff

/// Whether the entry's ASSEMBLED engine μ deviates from the vacuum identity at the classify
/// wavelength — used to decide the μ tab for a VIEW-ONLY preset, which has no magnetic toggle to read.
let assembledMagnetic (o : OpticalPropertiesWithDisp) : MagneticChoice =
    let m = o.muWithDisp.getMu classifyWavelength
    let ident (i : int) (j : int) : Complex = if i = j then Complex.One else Complex.Zero
    match [ for i in 0 .. 2 do for j in 0 .. 2 -> (m.[i, j] - ident i j).Magnitude ] |> List.exists (fun v -> v > 1e-12) with
    | true -> MagneticOn
    | false -> MagneticOff
