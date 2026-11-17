namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.LibraryFacets

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
