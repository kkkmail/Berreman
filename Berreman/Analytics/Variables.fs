namespace Analytics

open FSharp.Collections.ParallelSeq
open Berreman.MathNetNumericsMath
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Solvers
open Berreman.Dispersion

module Variables = 

    type Range<'T> = 
        {
            startValue : 'T
            endValue : 'T
            numberOfPoints : int
        }

        static member create n s e = { startValue = s; endValue = e; numberOfPoints = n }


    type RangedVariable = 
        | IncidenceAngleRange of Range<IncidenceAngle>
        | PolarizationRange of Range<Polarization>
        | EllipticityRange of Range<Ellipticity>
        | WaveLengthRange of Range<WaveLength>

        member this.length = 
            match this with 
            | IncidenceAngleRange v -> v.numberOfPoints
            | PolarizationRange v -> v.numberOfPoints
            | EllipticityRange v -> v.numberOfPoints
            | WaveLengthRange v -> v.numberOfPoints

        member this.name = 
            match this with 
            | IncidenceAngleRange _ -> "f"
            | PolarizationRange _ -> "p"
            | EllipticityRange _ -> "e"
            | WaveLengthRange _ -> "w (nm)"

        member this.value i = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle s)) = r.startValue
                let (IncidenceAngle (Angle e)) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | PolarizationRange r -> 
                let (Polarization (Angle s)) = r.startValue
                let (Polarization (Angle e)) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | EllipticityRange r -> 
                let (Ellipticity s) = r.startValue
                let (Ellipticity e) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | WaveLengthRange r -> 
                let (WaveLength s) = r.startValue
                let (WaveLength e) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)

        member this.plotValue i = 
            match this with 
            | IncidenceAngleRange _ -> (this.value i) |> toDegree
            | PolarizationRange _ -> (this.value i) |> toDegree
            | EllipticityRange _ -> this.value i
            | WaveLengthRange _ -> (this.value i) |> toNanometers

        member this.plotMinValue = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle s)) = r.startValue
                s |> toDegree
            | PolarizationRange r -> 
                let (Polarization (Angle s)) = r.startValue
                s |> toDegree
            | EllipticityRange r -> 
                let (Ellipticity s) = r.startValue
                s
            | WaveLengthRange r -> 
                let (WaveLength s) = r.startValue
                s |> toNanometers

        member this.plotMaxValue = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle e)) = r.endValue
                e |> toDegree
            | PolarizationRange r -> 
                let (Polarization (Angle e)) = r.endValue
                e |> toDegree
            | EllipticityRange r -> 
                let (Ellipticity e) = r.endValue
                e
            | WaveLengthRange r -> 
                let (WaveLength e) = r.endValue
                e |> toNanometers

        member this.plotPoints = [| for i in 0..this.length -> this.plotValue i |]


    let getWaveLengthValue (v : Range<WaveLength>) i = 
        (WaveLengthRange v).value i |> WaveLength


    let waveLengthPlotMinValue (v : Range<WaveLength>) = 
        (WaveLengthRange v).plotMinValue


    let waveLengthPlotMaxValue (v : Range<WaveLength>) = 
        (WaveLengthRange v).plotMaxValue


    let waveLengthPlotPoints (v : Range<WaveLength>) = 
        (WaveLengthRange v).plotPoints


    let getWaveLength (v : RangedVariable) i = 
        match v with
        | WaveLengthRange w -> getWaveLengthValue w i |> Some
        | _ -> None


    let getIncidenceAngle (v : RangedVariable) i = 
        match v with 
        | IncidenceAngleRange _ -> v.value i |> Angle |> IncidenceAngle |> Some
        | _ -> None


    let getPolarization (v : RangedVariable) i = 
        match v with 
        | PolarizationRange _ -> v.value i |> Angle |> Polarization |> Some
        | _ -> None


    let getEllipticity (v : RangedVariable) i = 
        match v with 
        | EllipticityRange _ -> v.value i |> Ellipticity |> Some
        | _ -> None


    /// Labels to distinguish varialbes in incident light. Refactor when nameof is available in F#.
    let private incidentLightLabels = ("w", "r", "i", "p", "e")


    let removeLightVariable x d = 
        let (w, _, i, p, e) = incidentLightLabels

        match x with
        | IncidenceAngleRange _ -> d |> List.choose (fun (k, v) -> if k = i then None else Some (k, v))
        | PolarizationRange _ -> d |> List.choose (fun (k, v) -> if k = p then None else Some (k, v))
        | EllipticityRange _ -> d |> List.choose (fun (k, v) -> if k = e then None else Some (k, v))
        | WaveLengthRange _ -> d |> List.choose (fun (k, v) -> if k = w then None else Some (k, v))


    type FixedInfo =
        {
            incidentLightInfo : IncidentLightInfo
            opticalSystem : OpticalSystemWithDisp
        }

        member private this.descriptionInfo =
            let (w, _, i, p, e) = incidentLightLabels

            [
                (i, this.incidentLightInfo.incidenceAngle.description)
                (p, this.incidentLightInfo.polarization.description)
                (e, this.incidentLightInfo.ellipticity.description)
                (w, this.incidentLightInfo.waveLength.description)
            ]

        member private __.toLightDescription d =
            d
            |> List.map snd
            |> List.fold (fun acc r -> acc + (if acc <> "" then ", " else "") + r) ""

        member this.getLightDescription (x : RangedVariable) =
            removeLightVariable x this.descriptionInfo |> this.toLightDescription

        member this.getLightDescription (x : RangedVariable, y : RangedVariable) =
            this.descriptionInfo
            |> removeLightVariable x
            |> removeLightVariable y
            |> this.toLightDescription

        member this.getDescription (x : RangedVariable) =
            let light = this.getLightDescription x

            match this.opticalSystem.description with
            | Some d -> light + ", " + d
            | None -> light

        member this.getDescription (x : RangedVariable, y : RangedVariable) =
            let light = this.getLightDescription (x, y)

            match this.opticalSystem.description with
            | Some d -> light + ", " + d
            | None -> light


    let calculate (f: FixedInfo) (x : RangedVariable) = 
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
            f.opticalSystem.getSystem w

        let getSolution i = OpticalSystemSolver(getLight i, getOpticalSystem i, SolverParameters.defaultValue).solution
        [| for i in 0..x.length -> (x.plotValue i, getSolution i) |]


    let calculate3D (f: FixedInfo) (x : RangedVariable) (y : RangedVariable) =
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
            f.opticalSystem.getSystem w

        let getSolution i j = OpticalSystemSolver(getLight i j, getOpticalSystem i j, SolverParameters.defaultValue).solution

        [| for i in 0..x.length -> i |]
        |> PSeq.map (fun i -> [| for j in 0..y.length -> (x.plotValue i, y.plotValue j, getSolution i j) |])
        |> Array.ofSeq


    let calculateOpticalProp 
        (c : OpticalPropertyComponent) 
        (t : OpticalTransformation)
        (i : Index, j : Index)
        (u : UseReIm)
        (o : OpticalPropertiesWithDisp) 
        (r : Range<WaveLength>) = 

        let f w = 
            let p =((o.getProperties w).opticalComponent c).[i, j] |> t.transform
            match u with 
            | UseRe -> p.Real
            | UseIm -> p.Imaginary

        let l = WaveLengthRange r
        [| for i in 0..l.length -> (l.plotValue i, getWaveLengthValue r i |> f) |]


    let calculateN11Re = calculateOpticalProp EpsComp SquareRoot (One, One) UseRe
    let calculateXi11Im = calculateOpticalProp EpsComp SquareRoot (One, One) UseIm

    let calculateN22Re = calculateOpticalProp EpsComp SquareRoot (Two, Two) UseRe
    let calculateXi22Im = calculateOpticalProp EpsComp SquareRoot (Two, Two) UseIm

    let calculateN33Re = calculateOpticalProp EpsComp SquareRoot (Three, Three) UseRe
    let calculateXi33Im = calculateOpticalProp EpsComp SquareRoot (Three, Three) UseIm

    let calculateRho11Im = calculateOpticalProp RhoComp MultByMillion (One, One) UseIm
    let calculateRho22Im = calculateOpticalProp RhoComp MultByMillion (Two, Two) UseIm
    let calculateRho33Im = calculateOpticalProp RhoComp MultByMillion (Three, Three) UseIm
