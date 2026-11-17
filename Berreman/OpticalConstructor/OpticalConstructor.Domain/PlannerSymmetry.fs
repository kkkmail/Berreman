namespace OpticalConstructor.Domain

open Berreman.MaterialProperties
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies

/// The Measurement Planner's layer-1 tables: what a crystal's symmetry allows and what a look at a plate
/// of it can see (spec 0050 S14.1).
module PlannerSymmetry =

    /// A count of independent tensor components or orientation angles (spec 0050 S14.1).
    type ComponentCount =
        | ComponentCount of int

        member this.value : int = let (ComponentCount count) = this in count

    type Anisotropy with

        /// How many independent principal indices the class's ε carries (spec 0050 S14.1).
        member this.independentIndexCount : ComponentCount =
            match this with
            | Isotropic -> ComponentCount 1
            | Uniaxial -> ComponentCount 2
            | Biaxial -> ComponentCount 3

    /// Whether a point group holds an inversion centre; derived, never stored (spec 0050 S14.1).
    type Centrosymmetry =
        | CentrosymmetricClass
        | NonCentrosymmetricClass

    /// Whether a point group's symmetry allows natural optical activity; derived, never stored (spec 0050 S14.1).
    type Gyrotropy =
        | GyrotropicClass
        | NonGyrotropicClass

    /// The 32 crystallographic point groups, each named by its crystal class (spec 0050 S14.1).
    type PointGroup =
        | TriclinicPedial
        | TriclinicPinacoidal
        | MonoclinicSphenoidal
        | MonoclinicDomatic
        | MonoclinicPrismatic
        | OrthorhombicDisphenoidal
        | OrthorhombicPyramidal
        | OrthorhombicDipyramidal
        | TetragonalPyramidal
        | TetragonalDisphenoidal
        | TetragonalDipyramidal
        | TetragonalTrapezohedral
        | DitetragonalPyramidal
        | TetragonalScalenohedral
        | DitetragonalDipyramidal
        | TrigonalPyramidal
        | TrigonalRhombohedral
        | TrigonalTrapezohedral
        | DitrigonalPyramidal
        | DitrigonalScalenohedral
        | HexagonalPyramidal
        | TrigonalDipyramidal
        | HexagonalDipyramidal
        | HexagonalTrapezohedral
        | DihexagonalPyramidal
        | DitrigonalDipyramidal
        | DihexagonalDipyramidal
        | CubicTetartoidal
        | CubicDiploidal
        | CubicGyroidal
        | CubicHextetrahedral
        | CubicHexoctahedral

        /// The Hermann-Mauguin symbol, a bar written as a leading minus (spec 0050 S14.1).
        member this.symbol : string =
            match this with
            | TriclinicPedial -> "1"
            | TriclinicPinacoidal -> "-1"
            | MonoclinicSphenoidal -> "2"
            | MonoclinicDomatic -> "m"
            | MonoclinicPrismatic -> "2/m"
            | OrthorhombicDisphenoidal -> "222"
            | OrthorhombicPyramidal -> "mm2"
            | OrthorhombicDipyramidal -> "mmm"
            | TetragonalPyramidal -> "4"
            | TetragonalDisphenoidal -> "-4"
            | TetragonalDipyramidal -> "4/m"
            | TetragonalTrapezohedral -> "422"
            | DitetragonalPyramidal -> "4mm"
            | TetragonalScalenohedral -> "-42m"
            | DitetragonalDipyramidal -> "4/mmm"
            | TrigonalPyramidal -> "3"
            | TrigonalRhombohedral -> "-3"
            | TrigonalTrapezohedral -> "32"
            | DitrigonalPyramidal -> "3m"
            | DitrigonalScalenohedral -> "-3m"
            | HexagonalPyramidal -> "6"
            | TrigonalDipyramidal -> "-6"
            | HexagonalDipyramidal -> "6/m"
            | HexagonalTrapezohedral -> "622"
            | DihexagonalPyramidal -> "6mm"
            | DitrigonalDipyramidal -> "-6m2"
            | DihexagonalDipyramidal -> "6/mmm"
            | CubicTetartoidal -> "23"
            | CubicDiploidal -> "m-3"
            | CubicGyroidal -> "432"
            | CubicHextetrahedral -> "-43m"
            | CubicHexoctahedral -> "m-3m"

        /// The ε form the class allows: cubic isotropic, tetragonal, trigonal and hexagonal uniaxial, the
        /// lower systems biaxial (spec 0050 S14.1).
        member this.anisotropy : Anisotropy =
            match this with
            | CubicTetartoidal
            | CubicDiploidal
            | CubicGyroidal
            | CubicHextetrahedral
            | CubicHexoctahedral -> Isotropic
            | TetragonalPyramidal
            | TetragonalDisphenoidal
            | TetragonalDipyramidal
            | TetragonalTrapezohedral
            | DitetragonalPyramidal
            | TetragonalScalenohedral
            | DitetragonalDipyramidal
            | TrigonalPyramidal
            | TrigonalRhombohedral
            | TrigonalTrapezohedral
            | DitrigonalPyramidal
            | DitrigonalScalenohedral
            | HexagonalPyramidal
            | TrigonalDipyramidal
            | HexagonalDipyramidal
            | HexagonalTrapezohedral
            | DihexagonalPyramidal
            | DitrigonalDipyramidal
            | DihexagonalDipyramidal -> Uniaxial
            | TriclinicPedial
            | TriclinicPinacoidal
            | MonoclinicSphenoidal
            | MonoclinicDomatic
            | MonoclinicPrismatic
            | OrthorhombicDisphenoidal
            | OrthorhombicPyramidal
            | OrthorhombicDipyramidal -> Biaxial

        /// Centrosymmetric for exactly the eleven Laue classes (spec 0050 S14.1).
        member this.centrosymmetry : Centrosymmetry =
            match this with
            | TriclinicPinacoidal
            | MonoclinicPrismatic
            | OrthorhombicDipyramidal
            | TetragonalDipyramidal
            | DitetragonalDipyramidal
            | TrigonalRhombohedral
            | DitrigonalScalenohedral
            | HexagonalDipyramidal
            | DihexagonalDipyramidal
            | CubicDiploidal
            | CubicHexoctahedral -> CentrosymmetricClass
            | TriclinicPedial
            | MonoclinicSphenoidal
            | MonoclinicDomatic
            | OrthorhombicDisphenoidal
            | OrthorhombicPyramidal
            | TetragonalPyramidal
            | TetragonalDisphenoidal
            | TetragonalTrapezohedral
            | DitetragonalPyramidal
            | TetragonalScalenohedral
            | TrigonalPyramidal
            | TrigonalTrapezohedral
            | DitrigonalPyramidal
            | HexagonalPyramidal
            | TrigonalDipyramidal
            | HexagonalTrapezohedral
            | DihexagonalPyramidal
            | DitrigonalDipyramidal
            | CubicTetartoidal
            | CubicGyroidal
            | CubicHextetrahedral -> NonCentrosymmetricClass

        /// Gyrotropic for 18 of the 21 non-centrosymmetric classes; -6, -6m2, -43m and every
        /// centrosymmetric class are not (spec 0050 S14.1; specs/0046/.manual/018 §3).
        member this.gyrotropy : Gyrotropy =
            match this with
            | TriclinicPedial
            | MonoclinicSphenoidal
            | MonoclinicDomatic
            | OrthorhombicDisphenoidal
            | OrthorhombicPyramidal
            | TetragonalPyramidal
            | TetragonalDisphenoidal
            | TetragonalTrapezohedral
            | DitetragonalPyramidal
            | TetragonalScalenohedral
            | TrigonalPyramidal
            | TrigonalTrapezohedral
            | DitrigonalPyramidal
            | HexagonalPyramidal
            | HexagonalTrapezohedral
            | DihexagonalPyramidal
            | CubicTetartoidal
            | CubicGyroidal -> GyrotropicClass
            | TrigonalDipyramidal
            | DitrigonalDipyramidal
            | CubicHextetrahedral
            | TriclinicPinacoidal
            | MonoclinicPrismatic
            | OrthorhombicDipyramidal
            | TetragonalDipyramidal
            | DitetragonalDipyramidal
            | TrigonalRhombohedral
            | DitrigonalScalenohedral
            | HexagonalDipyramidal
            | DihexagonalDipyramidal
            | CubicDiploidal
            | CubicHexoctahedral -> NonGyrotropicClass

        /// The angles that fix the principal axes where the lattice does not: three for triclinic, one for
        /// monoclinic, none above (spec 0050 S14.1).
        member this.orientationAngleCount : ComponentCount =
            match this with
            | TriclinicPedial
            | TriclinicPinacoidal -> ComponentCount 3
            | MonoclinicSphenoidal
            | MonoclinicDomatic
            | MonoclinicPrismatic -> ComponentCount 1
            | OrthorhombicDisphenoidal
            | OrthorhombicPyramidal
            | OrthorhombicDipyramidal
            | TetragonalPyramidal
            | TetragonalDisphenoidal
            | TetragonalDipyramidal
            | TetragonalTrapezohedral
            | DitetragonalPyramidal
            | TetragonalScalenohedral
            | DitetragonalDipyramidal
            | TrigonalPyramidal
            | TrigonalRhombohedral
            | TrigonalTrapezohedral
            | DitrigonalPyramidal
            | DitrigonalScalenohedral
            | HexagonalPyramidal
            | TrigonalDipyramidal
            | HexagonalDipyramidal
            | HexagonalTrapezohedral
            | DihexagonalPyramidal
            | DitrigonalDipyramidal
            | DihexagonalDipyramidal
            | CubicTetartoidal
            | CubicDiploidal
            | CubicGyroidal
            | CubicHextetrahedral
            | CubicHexoctahedral -> ComponentCount 0

        /// The 32 point groups, by crystal system from triclinic to cubic (spec 0050 S14.1).
        static member all : PointGroup list =
            [
                TriclinicPedial
                TriclinicPinacoidal
                MonoclinicSphenoidal
                MonoclinicDomatic
                MonoclinicPrismatic
                OrthorhombicDisphenoidal
                OrthorhombicPyramidal
                OrthorhombicDipyramidal
                TetragonalPyramidal
                TetragonalDisphenoidal
                TetragonalDipyramidal
                TetragonalTrapezohedral
                DitetragonalPyramidal
                TetragonalScalenohedral
                DitetragonalDipyramidal
                TrigonalPyramidal
                TrigonalRhombohedral
                TrigonalTrapezohedral
                DitrigonalPyramidal
                DitrigonalScalenohedral
                HexagonalPyramidal
                TrigonalDipyramidal
                HexagonalDipyramidal
                HexagonalTrapezohedral
                DihexagonalPyramidal
                DitrigonalDipyramidal
                DihexagonalDipyramidal
                CubicTetartoidal
                CubicDiploidal
                CubicGyroidal
                CubicHextetrahedral
                CubicHexoctahedral
            ]

    /// What a crystal record's tensor form allows: its anisotropy and the point groups whose tensors it
    /// holds (spec 0050 S14.1).
    type CrystalSymmetry =
        {
            anisotropy : Anisotropy
            pointGroups : PointGroup list
        }

        /// The independent principal indices of its anisotropy (spec 0050 S14.1).
        member this.independentIndexCount : ComponentCount = this.anisotropy.independentIndexCount

        /// The largest orientation angle count among its point groups, none without a group (spec 0050 S14.1).
        member this.orientationAngleCount : ComponentCount =
            this.pointGroups
            |> List.map (fun group -> group.orientationAngleCount)
            |> List.fold max (ComponentCount 0)

    type CrystalParameters with

        /// The record's symmetry: the uniaxial record holds classes 3/32/4/422/6/622 (diag(g11, g11, g33)),
        /// both triclinic records class 1 (spec 0050 S14.1).
        member this.symmetry : CrystalSymmetry =
            let pointGroups : PointGroup list =
                match this with
                | UniaxialCrystal _ ->
                    [
                        TrigonalPyramidal
                        TrigonalTrapezohedral
                        TetragonalPyramidal
                        TetragonalTrapezohedral
                        HexagonalPyramidal
                        HexagonalTrapezohedral
                    ]
                | TriclinicCrystal _
                | AbsorbingTriclinicCrystal _ -> [ TriclinicPedial ]

            { anisotropy = this.anisotropy; pointGroups = pointGroups }

    /// Below this a rotated marker's entry is zero: a 90 degree rotation leaves round-off near 1e-16 on the entries it
    /// zeroes (spec 0050 S14.1).
    [<Literal>]
    let private markerTolerance : float = 1.0e-9

    /// A principal axis of the frame the crystal records are written in (`CrystalPhysics.fs`; spec 0050 S14.1).
    type CrystalAxis =
        | FirstCrystalAxis
        | SecondCrystalAxis
        | ThirdCrystalAxis

        /// A probe crystal of permittivity diag(1, 4, 9) and no gyration, so each axis is recognised by its own value
        /// wherever a rotation carries it (spec 0050 S14.1).
        static member marker : OpticalProperties =
            let probe : TriclinicParameters =
                {
                    index1 = RefractionIndex 1.0
                    index2 = RefractionIndex 2.0
                    index3 = RefractionIndex 3.0
                    g11 = RhoValue 0.0
                    g22 = RhoValue 0.0
                    g33 = RhoValue 0.0
                    g23 = RhoValue 0.0
                    g13 = RhoValue 0.0
                    g12 = RhoValue 0.0
                }

            probe.opticalProperties

        /// The principal axis of a rotated marker that lies along the lab z-axis, the plate normal the system
        /// stratifies along (`orientForCut`); none when no principal axis lies there (spec 0050 S14.1).
        static member normalOf (oriented : OpticalProperties) : CrystalAxis option =
            let marker = CrystalAxis.marker

            let onNormal (diagonal : int) : bool =
                abs (oriented.eps.[2, 2].Real - marker.eps.[diagonal, diagonal].Real) < markerTolerance

            if oriented.eps.[0, 2].Magnitude >= markerTolerance || oriented.eps.[1, 2].Magnitude >= markerTolerance then None
            elif onNormal 0 then Some FirstCrystalAxis
            elif onNormal 1 then Some SecondCrystalAxis
            elif onNormal 2 then Some ThirdCrystalAxis
            else None

    /// Two principal axes in order, those of an off-diagonal tensor component (spec 0050 S14.1).
    type CrystalAxisPair =
        {
            firstAxis : CrystalAxis
            secondAxis : CrystalAxis
        }

    /// What a crystal record's parameter is in the tensors: a principal index on the axes it occupies (n_o on the
    /// 1- and 2-axes of a uniaxial crystal), a gyration component, or a component of the absorbing record's ε″
    /// (spec 0050 S14.1).
    type TensorComponent =
        | PrincipalIndexComponent of CrystalAxis list
        | GyrationDiagonalComponent of CrystalAxis list
        | GyrationOffDiagonalComponent of CrystalAxisPair
        | AbsorptionDiagonalComponent of CrystalAxis
        | AbsorptionOffDiagonalComponent of CrystalAxisPair

        /// The axes the component is on, a pair's two in order (spec 0050 S14.1).
        member this.axes : CrystalAxis list =
            match this with
            | PrincipalIndexComponent axes
            | GyrationDiagonalComponent axes -> axes
            | GyrationOffDiagonalComponent pair
            | AbsorptionOffDiagonalComponent pair -> [ pair.firstAxis; pair.secondAxis ]
            | AbsorptionDiagonalComponent axis -> [ axis ]

    /// One parameter of a crystal record and the tensor component it is (spec 0050 S14.1).
    type CrystalComponent =
        {
            name : ParameterName
            ``component`` : TensorComponent
        }

    type CrystalParameters with

        /// The record's parameters in their axes' order, each with its tensor component; the names are the
        /// `parameterAxes` names, never written again (spec 0050 S14.1).
        member this.components : CrystalComponent list =
            let principalAxes : CrystalAxis list = [ FirstCrystalAxis; SecondCrystalAxis; ThirdCrystalAxis ]

            // The records' off-diagonal order: 23, 13, 12 (`MuellerInverse.fs` axes).
            let offDiagonalPairs : CrystalAxisPair list =
                [
                    { firstAxis = SecondCrystalAxis; secondAxis = ThirdCrystalAxis }
                    { firstAxis = FirstCrystalAxis; secondAxis = ThirdCrystalAxis }
                    { firstAxis = FirstCrystalAxis; secondAxis = SecondCrystalAxis }
                ]

            let indices : TensorComponent list = principalAxes |> List.map (fun axis -> PrincipalIndexComponent [ axis ])

            let gyration : TensorComponent list =
                (principalAxes |> List.map (fun axis -> GyrationDiagonalComponent [ axis ]))
                @ (offDiagonalPairs |> List.map GyrationOffDiagonalComponent)

            let tensorComponents : TensorComponent list =
                match this with
                | UniaxialCrystal _ ->
                    [
                        PrincipalIndexComponent [ FirstCrystalAxis; SecondCrystalAxis ]
                        PrincipalIndexComponent [ ThirdCrystalAxis ]
                        GyrationDiagonalComponent [ FirstCrystalAxis; SecondCrystalAxis ]
                        GyrationDiagonalComponent [ ThirdCrystalAxis ]
                    ]
                | TriclinicCrystal _ -> indices @ gyration
                | AbsorbingTriclinicCrystal _ ->
                    indices
                    @ (principalAxes |> List.map AbsorptionDiagonalComponent)
                    @ (offDiagonalPairs |> List.map AbsorptionOffDiagonalComponent)
                    @ gyration

            List.map2
                (fun (axis : ParameterAxis<CrystalParameters>) (tensorComponent : TensorComponent) ->
                    { name = axis.name; ``component`` = tensorComponent })
                this.parameterAxes
                tensorComponents

    type SampleCut with

        /// The principal axis `orientForCut` carries onto the plate normal; the azimuth only turns the plate about its
        /// normal, so it is taken at zero (spec 0050 S14.1).
        member this.normalAxisOpt : CrystalAxis option =
            CrystalAxis.normalOf (orientForCut this (SampleAzimuth.degree 0.0<Berreman.Constants.deg>) CrystalAxis.marker)

    type CrystalOrientation with

        /// The principal axis the layer's rotation carries onto the plate normal, as `ResolvedLayer.getLayer` rotates
        /// it; `PrimaryAxes` is a z-cut (spec 0050 S14.1).
        member this.normalAxisOpt : CrystalAxis option = CrystalAxis.normalOf (CrystalAxis.marker.rotate this.toRotation)
