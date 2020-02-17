namespace Berreman

module Media =

    open MaterialProperties
    open Fields
    open Geometry


    type Thickness = 
        | Thickness of double
        | Infinity
        with
        static member nm t = t * Constants.nm |> Thickness
        static member mkm t = t * Constants.mkm |> Thickness
        static member mm t = t * Constants.mm |> Thickness
        member __.toInfinity () = Thickness.Infinity


    type Layer =
        {
            properties : OpticalProperties
            thickness : Thickness
        }

        member this.rotate (r : Rotation) : Layer = { this with properties = this.properties.rotate r}
        member this.rotatePiX = this.rotate Rotation.rotatePiX


    /// Use when upper system is coming with incident light EmField
    type ShortOpticalSystem = 
        {
            films : List<Layer>
            lower : OpticalProperties
        }


    type BaseOpticalSystem = 
        {
            description : string option
            upper : OpticalProperties
            films : List<Layer>
            lower : OpticalProperties
        }

        member this.fullSystem = 
            {
                description = this.description
                upper = this.upper
                films = this.films
                substrate = None
                lower = this.lower
            }

        member system.rotate (r : Rotation) : BaseOpticalSystem = 
            let newFilms = 
                system.films
                |> List.map (fun f -> { f with properties = f.properties.rotate r })
                |> List.rev

            { system with upper = system.upper.rotate r; films = newFilms; lower = system.lower.rotate r }

        member system.rotatePiX = system.rotate Rotation.rotatePiX
        member system.rotateX a = Rotation.rotateX a |> system.rotate
        member system.rotateY a = Rotation.rotateY a |> system.rotate
        member system.rotateZ a = Rotation.rotateZ a |> system.rotate


    and OpticalSystem = 
        {
            description : string option
            upper : OpticalProperties
            films : List<Layer>
            substrate : Layer option
            lower : OpticalProperties
        }

        member this.baseSystem = 
            match this.substrate with
            | None -> 
                {
                    description = this.description
                    upper = this.upper
                    films = this.films
                    lower = this.lower
                }
            | Some s -> 
                {
                    description = this.description
                    upper = this.upper
                    films = this.films
                    lower = s.properties
                }

        member system.rotate (r : Rotation) : OpticalSystem = 
            let sr = (system.baseSystem.rotate r).fullSystem

            match system.substrate with 
            | Some s -> { sr with substrate = s.rotate r |> Some}
            | None -> sr

        member this.rotatePiX = this.rotate Rotation.rotatePiX

