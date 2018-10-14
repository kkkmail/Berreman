﻿namespace Berreman
open MaterialProperties
open Fields
open Media

module Dispersion = 

    type EpsWithDisp = 
        | EpsWithDisp of (WaveLength -> Eps)
        | EpsWithoutDisp of Eps

        member this.getEps w = 
            match this with 
            | EpsWithDisp f -> f w
            | EpsWithoutDisp e -> e


    type Eps
        with
        member eps.dispersive = EpsWithoutDisp eps


    type MuWithDisp = 
        | MuWithDisp of (WaveLength -> Mu)
        | MuWithoutDisp of Mu

        member this.getMu w = 
            match this with 
            | MuWithDisp f -> f w
            | MuWithoutDisp e -> e

    type Mu
        with
        member mu.dispersive = MuWithoutDisp mu


    type RhoWithDisp = 
        | RhoWithDisp of (WaveLength -> Rho)
        | RhoWithoutDisp of Rho

        member this.getRho w = 
            match this with 
            | RhoWithDisp f -> f w
            | RhoWithoutDisp e -> e


    type Rho
        with
        member rho.dispersive = RhoWithoutDisp rho


    type OpticalPropertiesWithDisp = 
        {
            epsWithDisp : EpsWithDisp
            muWithDisp : MuWithDisp
            rhoWithDisp : RhoWithDisp
        }

        member this.getProperties w = 
            {
                eps = this.epsWithDisp.getEps w
                mu = this.muWithDisp.getMu w
                rho = this.rhoWithDisp.getRho w
            }


    type OpticalProperties
        with
        member this.dispersive = 
            {
                epsWithDisp = this.eps.dispersive
                muWithDisp = this.mu.dispersive
                rhoWithDisp = this.rho.dispersive
            }


    type LayerWithDisp = 
        {
            propertiesWithDisp : OpticalPropertiesWithDisp
            thickness : Thickness
        }

        member this.getLayer w = 
            {
                properties = this.propertiesWithDisp.getProperties w
                thickness = this.thickness
            }


    type Layer
        with
        member this.dispersive = 
            {
                propertiesWithDisp = this.properties.dispersive
                thickness = this.thickness
            }


    type OpticalSystemWithDisp = 
        {
            description : string option
            upperWithDisp : OpticalPropertiesWithDisp
            filmsWithDisp : List<LayerWithDisp>
            substrateWithDisp : LayerWithDisp option
            lowerWithDisp : OpticalPropertiesWithDisp
        }

        member this.getSystem w =
            {
                description = this.description
                upper = this.upperWithDisp.getProperties w
                films = this.filmsWithDisp |> List.map (fun f -> f.getLayer w)
                substrate = 
                    match this.substrateWithDisp with
                    | Some s -> s.getLayer w |> Some
                    | None -> None
                lower = this.lowerWithDisp.getProperties w
            }


    type OpticalSystem
        with
        member this.dispersive = 
            {
                description = this.description
                upperWithDisp = this.upper.dispersive
                filmsWithDisp = this.films |> List.map (fun f -> f.dispersive)
                substrateWithDisp = 
                    match this.substrate with
                    | Some s -> s.dispersive |> Some
                    | None -> None
                lowerWithDisp = this.lower.dispersive
            }
