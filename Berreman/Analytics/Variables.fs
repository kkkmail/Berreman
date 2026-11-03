namespace Analytics

open Berreman.Media
open FSharp.Collections.ParallelSeq
open Berreman.MathNetNumericsMath
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Solvers
open Berreman.Dispersion

module Variables =

    /// How many intervals a range is cut into: a sweep over it visits one point more, both ends included.
    type NumberOfPoints =
        | NumberOfPoints of int

        member this.value = let (NumberOfPoints n) = this in n


    /// The position of one point of a sweep, from 0 to its range's `NumberOfPoints` inclusive.
    type PointIndex =
        | PointIndex of int

        member this.value = let (PointIndex i) = this in i


    type Range<'T> =
        {
            startValue : 'T
            endValue : 'T
            numberOfPoints : NumberOfPoints
        }

        static member create (n : NumberOfPoints) (s : 'T) (e : 'T) : Range<'T> = { startValue = s; endValue = e; numberOfPoints = n }


    /// The value of a user-defined swept parameter, in the unit its `getSys` reads.
    type ArbitraryValue =
        | ArbitraryValue of float

        member this.value = let (ArbitraryValue v) = this in v


    /// The name a user-defined variable's chart axis carries.
    type VariableName =
        | VariableName of string

        member this.value = let (VariableName v) = this in v


    /// The factor from a user-defined variable's value to the number its chart axis shows.
    type PlotScale =
        | PlotScale of float

        member this.value = let (PlotScale v) = this in v


    type ArbitraryVariable =
        {
            variableName : VariableName
            range : Range<ArbitraryValue>
            scale : PlotScale
            getSys : OpticalSystem -> ArbitraryValue -> OpticalSystem
        }


    /// An incident-light parameter a sweep can vary; a chart's description leaves the varied ones out.
    type LightParameter =
        | LightWaveLength
        | LightIncidenceAngle
        | LightPolarization
        | LightEllipticity


    /// The text a chart prints above its plot: the system's description, then the light parameters the sweep holds fixed.
    type ChartDescription =
        | ChartDescription of string

        member this.value = let (ChartDescription d) = this in d

        /// Plotly.NET 5 no longer breaks a description line on "<br>", only on a newline
        /// (https://stackoverflow.com/questions/35185143/how-to-create-new-line-in-plot-ly-js-title).
        static member lineBreak : string = " " + System.Environment.NewLine


    /// Type to describe a single variable used in charts.
    type RangedVariable =
        | IncidenceAngleRange of Range<IncidenceAngle>
        | PolarizationRange of Range<Polarization>
        | EllipticityRange of Range<Ellipticity>
        | WaveLengthRange of Range<WaveLength>
        | WedgeAngleRange of Range<WedgeAngle>
        | ArbitraryVariableRange of ArbitraryVariable

        static member private toNanometers (w : float) : float = w / 1.0e-09

        member this.length =
            match this with
            | IncidenceAngleRange v -> v.numberOfPoints.value
            | PolarizationRange v -> v.numberOfPoints.value
            | EllipticityRange v -> v.numberOfPoints.value
            | WaveLengthRange v -> v.numberOfPoints.value
            | WedgeAngleRange v -> v.numberOfPoints.value
            | ArbitraryVariableRange v -> v.range.numberOfPoints.value

        member this.name =
            match this with
            | IncidenceAngleRange _ -> "f"
            | PolarizationRange _ -> "p"
            | EllipticityRange _ -> "e"
            | WaveLengthRange _ -> "w (nm)"
            | WedgeAngleRange _ -> "c"
            | ArbitraryVariableRange v -> v.variableName.value

        /// The incident-light parameter this variable sweeps, if it sweeps one.
        member this.lightParameter : LightParameter option =
            match this with
            | IncidenceAngleRange _ -> Some LightIncidenceAngle
            | PolarizationRange _ -> Some LightPolarization
            | EllipticityRange _ -> Some LightEllipticity
            | WaveLengthRange _ -> Some LightWaveLength
            | WedgeAngleRange _ -> None
            | ArbitraryVariableRange _ -> None

        member this.value (i : int) : float =
            let n = double this.length

            match this with
            | IncidenceAngleRange r ->
                let (IncidenceAngle (Angle s)) = r.startValue
                let (IncidenceAngle (Angle e)) = r.endValue
                s + (e - s) * (double i) / n
            | PolarizationRange r ->
                let (Polarization (Angle s)) = r.startValue
                let (Polarization (Angle e)) = r.endValue
                s + (e - s) * (double i) / n
            | EllipticityRange r ->
                let (Ellipticity s) = r.startValue
                let (Ellipticity e) = r.endValue
                s + (e - s) * (double i) / n
            | WaveLengthRange r ->
                let s = r.startValue.value
                let e = r.endValue.value
                s + (e - s) * (double i) / n |> double
            | WedgeAngleRange r ->
                let (WedgeAngle (Angle s)) = r.startValue
                let (WedgeAngle (Angle e)) = r.endValue
                s + (e - s) * (double i) / n
            | ArbitraryVariableRange v ->
                let (ArbitraryValue s) = v.range.startValue
                let (ArbitraryValue e) = v.range.endValue
                s + (e - s) * (double i) / n

        member this.plotValue (i : int) : float =
            match this with
            | IncidenceAngleRange _ -> (this.value i) |> Angle.radiansToDegrees
            | PolarizationRange _ -> (this.value i) |> Angle.radiansToDegrees
            | EllipticityRange _ -> this.value i
            | WaveLengthRange _ -> (this.value i) |> RangedVariable.toNanometers
            | WedgeAngleRange _ -> (this.value i) |> Angle.radiansToDegrees
            | ArbitraryVariableRange v -> (this.value i) * v.scale.value

        member this.plotMinValue =
            match this with
            | IncidenceAngleRange r -> r.startValue.value |> Angle.radiansToDegrees
            | PolarizationRange r -> r.startValue.value |> Angle.radiansToDegrees
            | EllipticityRange r -> r.startValue.value
            | WaveLengthRange r -> r.startValue.value / 1.0<meter> |> RangedVariable.toNanometers
            | WedgeAngleRange r -> r.startValue.value |> Angle.radiansToDegrees
            | ArbitraryVariableRange v -> v.range.startValue.value * v.scale.value

        member this.plotMaxValue =
            match this with
            | IncidenceAngleRange r -> r.endValue.value |> Angle.radiansToDegrees
            | PolarizationRange r -> r.endValue.value |> Angle.radiansToDegrees
            | EllipticityRange r -> r.endValue.value
            | WaveLengthRange r -> r.endValue.value / 1.0<meter> |> RangedVariable.toNanometers
            | WedgeAngleRange r -> r.endValue.value |> Angle.radiansToDegrees
            | ArbitraryVariableRange v -> v.range.endValue.value * v.scale.value

        member this.plotPoints = [| for i in 0..this.length -> this.plotValue i |]


    /// One point of a sweep: where it sits on the chart's x axis, and the solver's solution there.
    [<Softellect.Sys.NumericCarrier("an element of a sweep array (R3): the plotted x of every point the solver ran, read as a raw double by Plotly, the series adapter and the colorimetry integral")>]
    type SweepPoint =
        {
            plotValue : float
            solution : Solution
        }


    /// One point of a two-variable sweep: its plotted x and y, and the solver's solution there.
    [<Softellect.Sys.NumericCarrier("an element of a surface sweep array (R3): the plotted x and y of every grid point the solver ran, read as raw doubles by Plotly and the series adapter")>]
    type SurfacePoint =
        {
            xPlotValue : float
            yPlotValue : float
            solution : Solution
        }


    /// One point of a dispersion spectrum: the plotted wavelength and the optical property's component there.
    [<Softellect.Sys.NumericCarrier("an element of a dispersion spectrum array (R3): the plotted wavelength and the property component at every sweep point, read as raw doubles by Plotly and the series adapter")>]
    type DispersionPoint =
        {
            waveLengthPlotValue : float
            propertyValue : float
        }


    /// The wavelength at point `i` of a wavelength range, in the start value's own unit. `value i` is in
    /// METRES (the range is evaluated on `WaveLength.value`), so it is converted back before it is wrapped —
    /// wrapping the metre figure as nm (as this did before spec 0046) silently solved every wavelength sweep
    /// at ~10⁻⁷ nm while the x axis still read the right nm.
    let getWaveLengthValue (v : Range<WaveLength>) (i : PointIndex) : WaveLength =
        let metres = ((WaveLengthRange v).value i.value) * 1.0<meter>
        match v.startValue with
        | Nm _ -> WaveLength.nm (metres / nmToMeter)
        | Mkm _ -> WaveLength.mkm (metres / mkmToMeter)


    let getWaveLength (v : RangedVariable) (i : PointIndex) : WaveLength option =
        match v with
        | WaveLengthRange w -> getWaveLengthValue w i |> Some
        | _ -> None


    let getIncidenceAngle (v : RangedVariable) (i : PointIndex) : IncidenceAngle option =
        match v with
        | IncidenceAngleRange _ -> v.value i.value |> Angle |> IncidenceAngle |> Some
        | _ -> None


    let getPolarization (v : RangedVariable) (i : PointIndex) : Polarization option =
        match v with
        | PolarizationRange _ -> v.value i.value |> Angle |> Polarization |> Some
        | _ -> None


    let getEllipticity (v : RangedVariable) (i : PointIndex) : Ellipticity option =
        match v with
        | EllipticityRange _ -> v.value i.value |> Ellipticity |> Some
        | _ -> None


    let getWedgeAngle (v : RangedVariable) (i : PointIndex) : WedgeAngle option =
        match v with
        | WedgeAngleRange _ -> v.value i.value |> Angle |> WedgeAngle |> Some
        | _ -> None


    let getArbitraryVariable (v : RangedVariable) (i : PointIndex) : ArbitraryValue option =
        match v with
        | ArbitraryVariableRange _ -> v.value i.value |> ArbitraryValue |> Some
        | _ -> None


    /// Combination of incident light and optical system with dispersion.
    type FixedInfo =
        {
            incidentLightInfo : IncidentLightInfo
            opticalSystem : OpticalSystemWithDisp
        }

        /// The light parameters' descriptions, one per line, without those the `swept` variables vary.
        member private this.lightDescription (swept : RangedVariable list) : ChartDescription =
            let l = this.incidentLightInfo
            let varied = swept |> List.choose (fun v -> v.lightParameter)

            [
                (LightIncidenceAngle, l.incidenceAngle.description)
                (LightPolarization, l.polarization.description)
                (LightEllipticity, l.ellipticity.description)
                (LightWaveLength, l.waveLength.description)
            ]
            |> List.choose (fun (p, d) -> if List.contains p varied then None else Some d)
            |> List.fold (fun acc r -> acc + (if acc <> "" then ChartDescription.lineBreak else "") + r) ""
            |> ChartDescription

        member this.getLightDescription (x : RangedVariable) : ChartDescription =
            this.lightDescription [ x ]

        member this.getLightDescription (x : RangedVariable, y : RangedVariable) : ChartDescription =
            this.lightDescription [ x; y ]

        member private this.toFullDescription (light : ChartDescription) : ChartDescription =
            let lineBreak = ChartDescription.lineBreak

            match this.opticalSystem.description with
            | Some (OpticalSystemDescription d) -> d + lineBreak + light.value + lineBreak |> ChartDescription
            | None -> light.value + lineBreak |> ChartDescription

        member this.getDescription (x : RangedVariable) : ChartDescription =
            this.getLightDescription x |> this.toFullDescription

        member this.getDescription (x : RangedVariable, y : RangedVariable) : ChartDescription =
            this.getLightDescription (x, y) |> this.toFullDescription


    let calculate (f : FixedInfo) (x : RangedVariable) : SweepPoint[] =
        let l = f.incidentLightInfo

        let getValue d g i =
            match g x i with
            | Some v -> v
            | None -> d

        let getLight i =
            {
                waveLength = getValue l.waveLength getWaveLength i
                refractionIndex = l.refractionIndex
                incidenceAngle = getValue l.incidenceAngle getIncidenceAngle i
                polarization = getValue l.polarization getPolarization i
                ellipticity = getValue l.ellipticity getEllipticity i
            }

        let getOpticalSystem i =
            let w = getValue l.waveLength getWaveLength i
            let a = f.opticalSystem.getWedgeAngle() |> Option.defaultValue WedgeAngle.defaultValue
            let v = getValue a getWedgeAngle i
            let s = f.opticalSystem.getSystem w v

            match x, getArbitraryVariable x i with
            | ArbitraryVariableRange v, Some z -> v.getSys s z
            | _ -> s

        let getSolution i = OpticalSystemSolver(getLight i, getOpticalSystem i, SolverParameters.defaultValue).solution
        [| for i in 0..x.length -> { SweepPoint.plotValue = x.plotValue i; solution = getSolution (PointIndex i) } |]


    let calculate3D (f : FixedInfo) (x : RangedVariable) (y : RangedVariable) : SurfacePoint[][] =
        let l = f.incidentLightInfo

        let getValue d g i j =
            match g x i with
            | Some v -> v
            | None ->
                match g y j with
                | Some w -> w
                | None -> d

        let getLight i j =
            {
                waveLength = getValue l.waveLength getWaveLength i j
                refractionIndex = l.refractionIndex
                incidenceAngle = getValue l.incidenceAngle getIncidenceAngle i j
                polarization = getValue l.polarization getPolarization i j
                ellipticity = getValue l.ellipticity getEllipticity i j
            }

        let getOpticalSystem i j =
            let w = getValue l.waveLength getWaveLength i j
            let a = f.opticalSystem.getWedgeAngle() |> Option.defaultValue WedgeAngle.defaultValue
            let v = getValue a getWedgeAngle i j
            f.opticalSystem.getSystem w v

        let getSolution i j = OpticalSystemSolver(getLight i j, getOpticalSystem i j, SolverParameters.defaultValue).solution

        [| for i in 0..x.length -> i |]
        |> PSeq.map (fun i ->
            [|
                for j in 0..y.length ->
                    { SurfacePoint.xPlotValue = x.plotValue i; yPlotValue = y.plotValue j; solution = getSolution (PointIndex i) (PointIndex j) }
            |])
        |> Array.ofSeq


    let calculateOpticalProp
        (c : OpticalPropertyComponent)
        (t : OpticalTransformation)
        (i : Index, j : Index)
        (u : UseReIm)
        (o : OpticalPropertiesWithDisp)
        (r : Range<WaveLength>)
        : DispersionPoint[] =

        let f w =
            let p =((o.getProperties w).opticalComponent c)[i, j] |> t.transform
            match u with
            | UseRe -> p.Real
            | UseIm -> p.Imaginary

        let l = WaveLengthRange r
        [| for k in 0..l.length -> { waveLengthPlotValue = l.plotValue k; propertyValue = getWaveLengthValue r (PointIndex k) |> f } |]


    let calculateN11Re = calculateOpticalProp EpsComp SquareRoot (One, One) UseRe
    let calculateXi11Im = calculateOpticalProp EpsComp SquareRoot (One, One) UseIm

    let calculateN22Re = calculateOpticalProp EpsComp SquareRoot (Two, Two) UseRe
    let calculateXi22Im = calculateOpticalProp EpsComp SquareRoot (Two, Two) UseIm

    let calculateN33Re = calculateOpticalProp EpsComp SquareRoot (Three, Three) UseRe
    let calculateXi33Im = calculateOpticalProp EpsComp SquareRoot (Three, Three) UseIm

    let calculateRho11Im = calculateOpticalProp RhoComp MultByMillion (One, One) UseIm
    let calculateRho22Im = calculateOpticalProp RhoComp MultByMillion (Two, Two) UseIm
    let calculateRho33Im = calculateOpticalProp RhoComp MultByMillion (Three, Three) UseIm
