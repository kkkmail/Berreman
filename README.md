# Berreman

F# implementation of the Berreman matrix method for analyzing electromagnetic wave propagation through stratified (layered) optical media.

## Overview

The Berreman matrix is a 4x4 transfer matrix method used in computational electromagnetics to calculate reflection and transmission coefficients in multilayer thin films and anisotropic crystals. This library provides a comprehensive, type-safe implementation suitable for research and engineering applications in thin film optics, crystal optics, polarization analysis, and related fields.

## Features

- **Full Berreman 4x4 matrix implementation** for anisotropic and magneto-optically active media
- **Support for arbitrary optical properties**: electric permittivity (epsilon), magnetic permeability (mu), and magneto-electric coupling (rho)
- **Multilayer thin film calculations** with arbitrary number of layers
- **Thick plate and wedge substrate handling** with multiple internal reflections
- **Dispersive materials** with wavelength-dependent optical properties
- **Complete polarization analysis**: Stokes parameters, Jones vectors, Mueller matrices, ellipticity, azimuth
- **Active (magneto-optic) crystals**: planar, cubic (point group 23), type 3/4/6, and type 32/42/62 crystals
- **Gaussian beam support** via FFT-based propagation
- **Type-safe units of measure** (nm, um, mm, meter)
- **Interactive visualization** with Plotly.NET charts
- **Comprehensive test suite** with property-based testing

## Installation

### Requirements

- .NET 9.0 SDK
- x64 platform

### Building

```bash
dotnet build Berreman.sln -c Release
```

## Project Structure

```
Berreman/
├── Berreman/                    # Core library (Softellect.Berreman.Core)
│   └── Berreman/
│       ├── MathNetNumericsMath.fs   # Math abstractions and wrappers
│       ├── FourierTransform.fs      # FFT for Gaussian beams
│       ├── MatrixExp.fs             # Matrix exponential (Pade approximation)
│       ├── Constants.fs             # Physical constants and units
│       ├── Geometry.fs              # Vector/matrix types, rotations
│       ├── MaterialProperties.fs    # Optical properties (eps, mu, rho)
│       ├── Fields.fs                # Electromagnetic field definitions
│       ├── Media.fs                 # Optical media and systems
│       ├── BerremanMatrix.fs        # Core 4x4 matrix implementation
│       ├── Solvers.fs               # Reflection/transmission solvers
│       ├── Dispersion.fs            # Wavelength-dependent properties
│       └── FieldFunctions.fs        # Field analysis functions
├── OpticalProperties/           # Material definitions (Softellect.Berreman.OpticalProperties)
│   ├── Standard.fs                  # Standard materials (glass, crystals)
│   ├── Active.fs                    # Magneto-optic active crystals
│   └── Dispersive.fs                # Dispersive materials (Langasite, Silicon)
├── Analytics/                   # Visualization and analysis (Softellect.Berreman.Analytics)
│   ├── Variables.fs                 # Ranged variables for parametric studies
│   ├── StandardLightVariables.fs    # Common light configurations
│   ├── StandardSystems.fs           # Pre-defined optical systems
│   ├── Charting.fs                  # Plotly.NET integration
│   └── Examples/                    # Example scripts (see below)
├── BerremanTests/               # Test suite (xUnit, FsCheck)
├── BerremanRunner/              # Console application
└── BerremanModelGenerator/      # Model generation utility
```

## Core Concepts

### Optical Properties

Materials are defined by three 3x3 complex tensors:
- **Eps (epsilon)**: Electric permittivity tensor
- **Mu**: Magnetic permeability tensor
- **Rho**: Magneto-electric coupling tensor (for active crystals)

```fsharp
// Isotropic glass with refractive index 1.52
let glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.52)

// Uniaxial crystal
let uniaxial = OpticalProperties.uniaxialCrystal e11 e33

// Active planar crystal with magneto-electric effect
let active = OpticalProperties.planarCrystal e11 e33 g12
```

### Optical Systems

An optical system consists of:
- **Upper medium**: Semi-infinite medium where light originates
- **Films**: List of thin film layers with thickness
- **Substrate**: Optional thick plate or wedge
- **Lower medium**: Semi-infinite exit medium

```fsharp
let system = {
    description = Some "Multilayer coating"
    upper = OpticalProperties.vacuum
    films = [
        { properties = glass; thickness = Thickness.nm 100.0 }
        { properties = vacuum; thickness = Thickness.nm 150.0 }
    ]
    lower = OpticalProperties.vacuum
}
```

### Incident Light

Light is characterized by:
- **Wavelength**: With unit safety (nm, um, mm)
- **Incidence angle**: From normal (0 degrees) to grazing
- **Polarization**: s, p, or arbitrary angle
- **Ellipticity**: For elliptically polarized light

```fsharp
let light = {
    wavelength = WaveLength.nm 600.0
    incidenceAngle = IncidenceAngle.normal
    polarization = Polarization.s
    ellipticity = Ellipticity.zero
}
```

### Output Functions

Available output quantities:
- `R`, `T`: Total reflectance and transmittance
- `Rs`, `Rp`, `Ts`, `Tp`: s- and p-polarized components
- `Is`, `Ip`: Incident intensity components
- `AzimuthR`, `AzimuthT`: Polarization azimuth angles
- `EllipticityR`, `EllipticityT`: Polarization ellipticity

## Examples

The `Analytics/Examples/` folder contains F# script files demonstrating various optical scenarios. To run an example:

1. Build the solution in Release mode
2. Open the `.fsx` file in an F# interactive environment (FSI)
3. Execute the script

### Glass.fsx

**Simple glass substrate and film analysis**

Demonstrates the difference between treating glass as a thin film vs. a thick substrate. Compares reflectance and transmittance as functions of incidence angle for a glass layer (n=1.78) at 600nm wavelength.

Key features:
- Thin film interference effects vs. incoherent thick plate behavior
- Incidence angle dependence (0 to 90 degrees)
- Wavelength dependence (300-700nm)

### Glass_Dispersive.fsx

**Dispersive glass with wavelength-dependent refractive index**

Uses a Sellmeier-type dispersion formula for the ordinary refractive index:
```
n^2 = 1 + 1.43*lambda^2/(lambda^2 - 0.073^2) + 0.65*lambda^2/(lambda^2 - 0.12^2)
```

Demonstrates how to define custom dispersive materials and plot refractive index vs. wavelength.

### MultilayerThinFilm.fsx

**Multilayer dielectric coating (anti-reflection)**

A 40-layer alternating glass/vacuum quarter-wave stack designed for 600nm. Each layer is lambda/(4n) thick for its material:
- Glass layers (n=1.52): 98.7nm
- Vacuum layers (n=1.0): 150nm

Shows characteristic interference patterns in reflectance vs. wavelength and incidence angle.

### MultilayerThinFilm_02.fsx

**High-reflectance multilayer mirror**

100 layer pairs of alternating high/low index materials (n=1.75/1.50) on glass substrate. Demonstrates:
- High reflectance at design wavelength
- Spectral bandwidth engineering
- Angular dependence of multilayer mirrors

### MultilayerThinFilm_EUV.fsx

**EUV (Extreme Ultraviolet) multilayer mirror**

100 Mo/Si layer pairs for 10.6nm (EUV) wavelength using complex refractive indices:
- Molybdenum: absorption and phase shift at EUV
- Silicon: spacer layers

Used in EUV lithography mirrors and X-ray optics.

### MultilayerThinFilm_EUV__002.fsx

**Custom EUV mirror parameters**

Similar to above but with user-defined delta and beta values for the complex refractive index:
```
n = (1 - delta) + i*beta
```

### LangasiteOnSilicon.fsx

**Dispersive active crystal (Langasite) on Silicon substrate**

Langasite (La3Ga5SiO14) is an optically active piezoelectric crystal used in sensors. This example:
- Uses full dispersive optical properties from literature
- Shows thin film vs. thick substrate comparison
- Demonstrates wavelength-dependent behavior (250-600nm)

### ActiveCrystal.fsx

**Magneto-optically active planar crystal plate**

Demonstrates optical activity (polarization rotation) in crystals with non-zero rho tensor:
- Uniaxial crystal (n11=2.315, n33=2.226)
- Variable gyration coefficient g12
- Plots s- and p-polarized transmission vs. g12

### ActiveCrystalComparison.fsx

**Comparison of different active crystal types**

Compares three crystal symmetry classes:
- **Planar crystal**: rho has only g12 component
- **Type 3,4,6 crystal**: rho has g11 and g33 components
- **Type 32,42,62 crystal**: rho has g11, g12, and g33 components

Shows transmitted azimuth and ellipticity as functions of incidence angle.

### TotalRefl_Glass.fsx

**Total internal reflection at glass/vacuum interface**

Light traveling from glass (n=1.50) to vacuum demonstrates:
- Critical angle calculation
- Brewster angle identification
- s- and p-polarized reflectance behavior

### Wedge_Glass.fsx

**Glass wedge prism**

Analyzes a glass prism with 40-degree wedge angle:
- Reflectance and transmittance vs. wedge angle (0-85 degrees)
- Handles multiple internal reflections

### Wedge_BiaxialCrystal.fsx

**Biaxial crystal wedge**

Similar to glass wedge but with anisotropic biaxial crystal (nx=1.5, ny=1.65, nz=1.75):
- Demonstrates birefringence effects in wedge geometry
- 45-degree polarized input to excite both eigenmodes

### Wedge_ActiveCrystal.fsx

**Active planar crystal wedge**

Combines wedge geometry with optical activity:
- 23-degree wedge of planar active crystal
- Variable gyration coefficient analysis
- Useful for polarimetric device design

### Wedge_ActiveCrystalComparison.fsx

**Comparison of active crystal wedges**

Compares wedge behavior for three crystal symmetry classes:
- Transmitted intensity and polarization state
- Azimuth and ellipticity of transmitted light
- 23-degree wedge geometry

## API Reference

### Key Types

| Type | Description |
|------|-------------|
| `WaveLength` | Wavelength with units (nm, um, mm) |
| `Thickness` | Layer thickness with units |
| `IncidenceAngle` | Angle of incidence |
| `Polarization` | Linear polarization angle |
| `Ellipticity` | Elliptical polarization parameter |
| `OpticalProperties` | Material properties (eps, mu, rho) |
| `Film` | Thin film layer |
| `OpticalSystem` | Complete optical system |
| `IncidentLightInfo` | Light source specification |

### Key Functions

```fsharp
// Create optical system info
let info = { incidentLightInfo = light; opticalSystem = system.dispersive }

// Plot results
plot info [ R; T ] incidenceAngleRange
plot info [ Rs; Rp; Ts; Tp ] wavelengthRange
plotComparison [ system1; system2 ] [ R; T ] variable
plot3D info [ R ] xRange yRange
```

## Dependencies

- **MathNet.Numerics**: Linear algebra and numerical methods (customized version included)
- **Plotly.NET**: Interactive charting
- **FSharp.Collections.ParallelSeq**: Parallel processing
- **xUnit**: Testing framework
- **FsCheck**: Property-based testing
- **FluentAssertions**: Test assertions

## License

This project is licensed under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for details.

Individual components may have different licenses:
- MatrixExp.fs: LGPL (ported from C code)
- Core library packages: MIT license

## Author

**Konstantin Konstantinov**
Softellect Systems, Inc.

## Repository

https://github.com/kkkmail/Berreman

## References

The Berreman matrix method is described in:
- D. W. Berreman, "Optics in Stratified and Anisotropic Media: 4x4-Matrix Formulation," J. Opt. Soc. Am. 62, 502-510 (1972)

## Version

Current version: 9.0.100.5 (targeting .NET 9.0)
