namespace Berreman

open System
//open ExtremeNumericsMath
open MathNetNumericsMath

open Geometry
open Fields
open Media
open BerremanMatrix

module Solvers =

    type SolverParameters =
        {
            /// Number of reflections to use when there is a thick substrate plate.
            numberOfReflections : int
        }

        static member defaultValue =
            {
                numberOfReflections = 5
            }


    type InputData =
        | InfoBased of IncidentLightInfo * BaseOpticalSystem
        | EmFieldBased of EmField * ShortOpticalSystem


    type BaseOpticalSystemSolver private (input : InputData) =
        let i, m1, m2, waveLength, n1SinFita, films, upper, lower =
            match input with
            | InfoBased (info, system) ->
                let bm = BerremanMatrix.create info.n1SinFita
                EmField.create (info, system.upper), bm system.upper, bm system.lower, info.waveLength, info.n1SinFita, system.films, system.upper, system.lower
            | EmFieldBased (e, system) ->
                let bm = BerremanMatrix.create e.n1SinFita
                e, bm e.opticalProperties, bm system.lower, e.waveLength, e.n1SinFita, system.films, e.opticalProperties, system.lower

        let (BerremanMatrixPropagated p) = BerremanMatrixPropagated.propagate (films, i)
        let b1 = m1.eigenBasis ()
        let b2 = m2.eigenBasis ()

        // Generated, do not modify.
        let coeffTblVal =
            [
                [
                    b1.up.e0.[0] * p.[0, 0] + b1.up.e0.[1] * p.[0, 1] + b1.up.e0.[2] * p.[0, 2] + b1.up.e0.[3] * p.[0, 3]
                    b1.up.e1.[0] * p.[0, 0] + b1.up.e1.[1] * p.[0, 1] + b1.up.e1.[2] * p.[0, 2] + b1.up.e1.[3] * p.[0, 3]
                    -b2.down.e0.[0]
                    -b2.down.e1.[0]
                ]
                [
                    b1.up.e0.[0] * p.[1, 0] + b1.up.e0.[1] * p.[1, 1] + b1.up.e0.[2] * p.[1, 2] + b1.up.e0.[3] * p.[1, 3]
                    b1.up.e1.[0] * p.[1, 0] + b1.up.e1.[1] * p.[1, 1] + b1.up.e1.[2] * p.[1, 2] + b1.up.e1.[3] * p.[1, 3]
                    -b2.down.e0.[1]
                    -b2.down.e1.[1]
                ]
                [
                    b1.up.e0.[0] * p.[2, 0] + b1.up.e0.[1] * p.[2, 1] + b1.up.e0.[2] * p.[2, 2] + b1.up.e0.[3] * p.[2, 3]
                    b1.up.e1.[0] * p.[2, 0] + b1.up.e1.[1] * p.[2, 1] + b1.up.e1.[2] * p.[2, 2] + b1.up.e1.[3] * p.[2, 3]
                    -b2.down.e0.[2]
                    -b2.down.e1.[2]
                ]
                [
                    b1.up.e0.[0] * p.[3, 0] + b1.up.e0.[1] * p.[3, 1] + b1.up.e0.[2] * p.[3, 2] + b1.up.e0.[3] * p.[3, 3]
                    b1.up.e1.[0] * p.[3, 0] + b1.up.e1.[1] * p.[3, 1] + b1.up.e1.[2] * p.[3, 2] + b1.up.e1.[3] * p.[3, 3]
                    -b2.down.e0.[3]
                    -b2.down.e1.[3]
                ]
            ]
            |> ComplexMatrix.create

        let cfmVal = coeffTblVal.inverse
        let detInv = cfmVal.determinant

        let freeTblVal =
            [
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[0, 1] + b1.down.e0.[3] * p.[0, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.x * p.[0, 1] + b1.down.e1.[0] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.x * p.[0, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.y * p.[0, 1] + b1.down.e1.[2] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.y * p.[0, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[1, 1] + b1.down.e0.[3] * p.[1, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.x * p.[1, 1] + b1.down.e1.[0] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.x * p.[1, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.y * p.[1, 1] + b1.down.e1.[2] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.y * p.[1, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[2, 1] + b1.down.e0.[3] * p.[2, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.x * p.[2, 1] + b1.down.e1.[0] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.x * p.[2, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.y * p.[2, 1] + b1.down.e1.[2] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.y * p.[2, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[3, 1] + b1.down.e0.[3] * p.[3, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.x * p.[3, 1] + b1.down.e1.[0] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.x * p.[3, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.y * p.[3, 1] + b1.down.e1.[2] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.y * p.[3, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
            ]
            |> ComplexVector.create

        let ehr, eht =
            match Double.IsNaN(detInv.Real), Double.IsNaN(detInv.Imaginary) with
            | false, false ->
                let sol = cfmVal * freeTblVal

                let ehr =
                    [
                        b1.up.e0.[0] * sol.[0] + b1.up.e1.[0] * sol.[1]
                        b1.up.e0.[1] * sol.[0] + b1.up.e1.[1] * sol.[1]
                        b1.up.e0.[2] * sol.[0] + b1.up.e1.[2] * sol.[1]
                        b1.up.e0.[3] * sol.[0] + b1.up.e1.[3] * sol.[1]
                    ]
                    |> ComplexVector4.create

                let eht =
                    [
                        b2.down.e0.[0] * sol.[2] + b2.down.e1.[0] * sol.[3]
                        b2.down.e0.[1] * sol.[2] + b2.down.e1.[1] * sol.[3]
                        b2.down.e0.[2] * sol.[2] + b2.down.e1.[2] * sol.[3]
                        b2.down.e0.[3] * sol.[2] + b2.down.e1.[3] * sol.[3]
                    ]
                    |> ComplexVector4.create

                ehr, eht
            | _ ->
                // This is a case of total reflection.

                let coeffTblVal2x2 =
                    [
                        [ coeffTblVal.[0,0]; coeffTblVal.[0,1] ]
                        [ coeffTblVal.[1,0]; coeffTblVal.[1,1] ]
                    ]
                    |> ComplexMatrix.create

                let det2 = coeffTblVal2x2.determinant
                let cfmVal2x2 = coeffTblVal2x2.inverse

                let c20 = coeffTblVal.[2,0]
                let c02 = coeffTblVal.[0,2]

                let sol0 = freeTblVal.[2] / coeffTblVal.[2,0]
                let sol1 = freeTblVal.[1] / coeffTblVal.[1,1]


                let ehr =
                    [
                        b1.up.e0.[0] * sol0 + b1.up.e1.[0] * sol1
                        b1.up.e0.[1] * sol0 + b1.up.e1.[1] * sol1
                        b1.up.e0.[2] * sol0 + b1.up.e1.[2] * sol1
                        b1.up.e0.[3] * sol0 + b1.up.e1.[3] * sol1
                    ]
                    |> ComplexVector4.create

                let eht =
                    [
                        cplx 0.0
                        cplx 0.0
                        cplx 0.0
                        cplx 0.0
                    ]
                    |> ComplexVector4.create

                ehr, eht

        let r =
            {
                waveLength = waveLength
                n1SinFita = n1SinFita
                opticalProperties = upper
                eh = ehr |> BerremanFieldEH
            }.toEmField ()

        let t =
            {
                waveLength = waveLength
                n1SinFita = n1SinFita
                opticalProperties = lower
                eh = eht |> BerremanFieldEH
            }.toEmField ()

        let ems =
            {
                incident = i
                reflected = r
                transmitted = t
            }

        member _.cfm = cfmVal
        member _.emSys = ems

        new (info : IncidentLightInfo, system : BaseOpticalSystem) = BaseOpticalSystemSolver (InfoBased (info, system))
        new (emf : EmField, system : ShortOpticalSystem) = BaseOpticalSystemSolver (EmFieldBased (emf, system))


    type Solution =
        | Single of BaseOpticalSystemSolver
        | Multiple of MultipleEmFieldSystem

        member sol.emSys =
            match sol with
            | Single s -> s.emSys
            | Multiple m ->
                let d =
                    {
                        EmField.getDefaultValue m.incident.waveLength
                            with
                                n1SinFita = m.incident.n1SinFita
                                opticalProperties = m.incident.opticalProperties
                    }

                let choose mapper =
                    m.rt |> List.map mapper |> List.choose id |> List.tryHead |> Option.defaultValue d

                {
                    incident = m.incident
                    reflected = choose (fun e -> e.reflected)
                    transmitted = choose (fun e -> e.transmitted)
                }

        static member create i rt =
            {
                incident = i
                rt = rt
            }
            |> Multiple


    /// First step is when we solve the base system where the upper semi-indefinite media is a substrate.
    /// Down step is the transmitted light from the first step OR _______ ...
    /// Up step is the light reflected from lower semi-infinite media going up toward thin films.
    /// We use BaseOpticalSystemSolver at each step.
    /// The steps go as follows: FirstStep, DownStep, UpStep, DownStep, UpStep, ...
    type SolutionNextStep =
        | DownStep
        | UpStep

        member this.next =
            match this with
            | DownStep -> UpStep
            | UpStep -> DownStep


    type OpticalSystemSolver (info : IncidentLightInfo, system: OpticalSystem, parameters : SolverParameters) =
        let sol =
            match system.substrate with
            | None -> BaseOpticalSystemSolver(info, system.baseSystem) |> Single
            | Some substrate ->
                match substrate with
                | Plate s ->
                    let firstSys : BaseOpticalSystem = system.baseSystem
                    let firstOut (ems : EmFieldSystem) : EmField = ems.transmitted.propagate s
                    let firstAcc (ems : EmFieldSystem) : (EmField option * EmField option) = (Some ems.reflected, None)

                    let downSys : ShortOpticalSystem =
                        {
                            films = []
                            lower = system.lower
                        }

                    let downOut (ems : EmFieldSystem) : EmField = ems.reflected.rotatePiX.propagate s.rotatePiX
                    let downAcc (ems : EmFieldSystem) : (EmField option * EmField option) = (None, Some ems.transmitted)

                    let upSys : ShortOpticalSystem =
                        {
                            films = system.rotatePiX.films |> List.rev // TODO Make it clear what's going on here.
                            lower = system.upper.rotatePiX
                        }

                    let upOut (ems : EmFieldSystem) : EmField = ems.reflected.rotatePiX.propagate s
                    let upAcc (ems : EmFieldSystem) : (EmField option * EmField option) = (Some (ems.transmitted.rotatePiX), None)

                    let first, incidentLight =
                        let ems = BaseOpticalSystemSolver(info, firstSys).emSys
                        ((DownStep, ems |> firstOut), [ ems |> firstAcc ]), ems.incident

                    let makeStep ((nextStep : SolutionNextStep, emf : EmField), acc) =
                        match nextStep with
                        | DownStep ->
                            let ems = BaseOpticalSystemSolver(emf, downSys).emSys
                            (UpStep, ems |> downOut), (ems |> downAcc) :: acc
                        | UpStep ->
                            let ems = BaseOpticalSystemSolver(emf, upSys).emSys
                            (DownStep, ems |> upOut), (ems |> upAcc) :: acc


                    [ for i in 0..parameters.numberOfReflections -> i ]
                    |> List.fold (fun next _ -> makeStep next) (first)
                    |> snd
                    |> List.map (fun (r, t) -> { reflected = r; transmitted = t })
                    |> Solution.create incidentLight
                | Wedge s ->
                    let (WedgeAngle angle) = s.angle
                    let ems = BaseOpticalSystemSolver(info, system.baseSystem).emSys
                    let start = (ems.transmitted.propagate s, [ (Some ems.reflected, None) ])

                    let downSys : ShortOpticalSystem =
                        {
                            films = []
                            lower = system.lower.rotateY angle
                        }

                    let transmit (emf : EmField, acc) =
                        let sys = BaseOpticalSystemSolver(emf.rotateY angle, downSys).emSys
                        (None, sys.transmitted.rotateY (-angle) |> Some) :: acc

                    let r =
                        start
                        |> transmit
                        |> List.map (fun (r, t) -> { reflected = r; transmitted = t })
                        |> Solution.create ems.incident

                    r

        // These must be functions as otherwise we will have an infinite loop.
        let solS () = OpticalSystemSolver (info.s, system, parameters)
        let solP () = OpticalSystemSolver (info.p, system, parameters)

        let getMuellerMatrix rt =
            let sS = solS()
            let sP = solP()

            let getSingleField (e : BaseOpticalSystemSolver) =
                match rt with
                | Reflected -> e.emSys.reflected
                | Transmitted -> e.emSys.transmitted

            let getMultipleField (e : ReflectedTransmitted) =
                match rt with
                | Reflected -> e.reflected
                | Transmitted -> e.transmitted

            match sS.solution, sP.solution with
            | Single s, Single p -> MuellerMatrix.fromEmFields (getSingleField s) (getSingleField p)
            | Multiple s, Multiple p ->
                let m =
                    List.zip (s.rt |> List.map getMultipleField) (p.rt |> List.map getMultipleField)
                    |> List.map (fun (a, b) -> match (a, b) with | (Some x, Some y) -> Some (x, y) | _ -> None )
                    |> List.choose id
                    |> List.map (fun (a, b) -> MuellerMatrix.fromEmFields a b)
                    |> List.sum
                m
            | _ -> failwith "Invalid combination of parameters in getMuellerMatrix!"

        member _.solution = sol
        member _.solutionS() = solS()
        member _.solutionP() = solP()
        member _.muellerMatrixR() = getMuellerMatrix Reflected
        member _.muellerMatrixT() = getMuellerMatrix Transmitted

        new (info : IncidentLightInfo, system: OpticalSystem) = OpticalSystemSolver (info, system, SolverParameters.defaultValue)
