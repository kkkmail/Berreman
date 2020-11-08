namespace Berreman

open System
open MathNetNumericsMath

open Geometry
open Fields
open Media
open BerremanMatrix
open MaterialProperties

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


    /// Generated, do not modify.
    let private getCoeffTblVal (BerremanMatrixPropagated p) (b1 : FullEigenBasis) (b2 : FullEigenBasis) =
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


    /// Generated, do not modify.
    let private getFreeTblVal (BerremanMatrixPropagated p) (i : EmComponent) (b1 : FullEigenBasis) (b2 : FullEigenBasis) =
        [
            ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[0, 1] + b1.down.e0.[3] * p.[0, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.x * p.[0, 1] + b1.down.e1.[0] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.x * p.[0, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.y * p.[0, 1] + b1.down.e1.[2] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.y * p.[0, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
            ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[1, 1] + b1.down.e0.[3] * p.[1, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.x * p.[1, 1] + b1.down.e1.[0] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.x * p.[1, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.y * p.[1, 1] + b1.down.e1.[2] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.y * p.[1, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
            ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[2, 1] + b1.down.e0.[3] * p.[2, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.x * p.[2, 1] + b1.down.e1.[0] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.x * p.[2, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.y * p.[2, 1] + b1.down.e1.[2] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.y * p.[2, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
            ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[3, 1] + b1.down.e0.[3] * p.[3, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.x * p.[3, 1] + b1.down.e1.[0] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.x * p.[3, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.y * p.[3, 1] + b1.down.e1.[2] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.y * p.[3, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
        ]
        |> ComplexVector.create


    let private getRT upper lower films w (i : EmComponent) =
        let n1SinFita = i.n1SinFita
        let m1 : BerremanMatrix = BerremanMatrix.create upper n1SinFita
        let m2 : BerremanMatrix = BerremanMatrix.create lower n1SinFita
        let b1 = m1.eigenBasis()
        let b2 = m2.eigenBasis()
        let p = BerremanMatrixPropagated.propagate (films, i, w)

        let coeffTblVal = getCoeffTblVal p b1 b2
        let freeTblVal = getFreeTblVal p i b1 b2
        let cfmVal = coeffTblVal.inverse
        let detInv = cfmVal.determinant

        let getEH (sol : ComplexVector) =
            let ehr0 = EmComponent.create b1.up.evv0 (sol.[0]) n1SinFita upper
            let ehr1 = EmComponent.create b1.up.evv1 (sol.[1]) n1SinFita upper

            let eht0 = EmComponent.create b2.down.evv0 (sol.[2]) n1SinFita lower
            let eht1 = EmComponent.create b2.down.evv1 (sol.[3]) n1SinFita lower

            let ehr = [ ehr0; ehr1 ]
            let eht = [ eht0; eht1 ]
            ehr, eht

        match Double.IsNaN(detInv.Real), Double.IsNaN(detInv.Imaginary) with
        | false, false -> cfmVal * freeTblVal |> getEH
        | _ ->
            // This is a case of total reflection.
            // The matrix is degenerate and we can't inverse it.
            [ freeTblVal.[2] / coeffTblVal.[2,0] ; freeTblVal.[1] / coeffTblVal.[1,1]; cplx 0.0; cplx 0.0 ]
            |> ComplexVector.create
            |> getEH


    type BaseOpticalSystemSolver private (input : InputData) =
        let i, waveLength, films, upper, lower =
            match input with
            | InfoBased (info, system) ->
                EmField.create (info, system.upper), info.waveLength, system.films, system.upper, system.lower
            | EmFieldBased (e, system) ->
                e, e.waveLength, system.films, e.opticalProperties, system.lower

        let getRT = getRT upper lower films waveLength
        let (ehr, eht) = i.emComponents |> List.map getRT |> List.unzip

        let r =
            {
                waveLength = waveLength
                opticalProperties = upper
                emComponents = ehr |> List.concat
            }

        let t =
            {
                waveLength = waveLength
                opticalProperties = lower
                emComponents = eht |> List.concat
            }

        let ems =
            {
                incident = i
                reflected = r
                transmitted = t
            }

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

                    let firstOut (ems : EmFieldSystem) : EmField =
                        let a = ems.transmitted.propagate s
                        a

                    let firstAcc (ems : EmFieldSystem) : (EmField option * EmField option) =
                        let a = (Some ems.reflected, None)
                        a

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
                        let (f, i) = ((DownStep, ems |> firstOut), [ ems |> firstAcc ]), ems.incident
                        f, i

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

                    let transmit (emf : EmField, acc) =
                        let emfRot = emf.rotateY (-angle)

                        let downSys : ShortOpticalSystem =
                            {
                                films = []
                                lower = system.lower.rotateY (-angle)
                            }

                        let sys = BaseOpticalSystemSolver(emfRot, downSys).emSys
                        let tRot = sys.transmitted
                        let tRot1 = sys.transmitted.rotateY (-angle)
                        let tRot2 = sys.transmitted.rotateY angle
                        (None, tRot |> Some) :: acc

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
