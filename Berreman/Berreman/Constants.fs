namespace Berreman
module Constants =
    open System.Numerics
    open MathNetNumericsMath

    [<Measure>]
    type meter


    [<Measure>]
    type mm


    [<Measure>]
    type mkm


    [<Measure>]
    type nm


    /// Angle in degrees.
    [<Measure>]
    type deg


    let oneNanometer = 1.0<nm>
    let oneMicrometer = 1.0<mkm>
    let oneMillimeter = 1.0<mm>


    /// Multiplicative constant to convert from nm to meter.
    let nmToMeter: double<meter/nm> = 1.0e-9<meter/nm>

    /// Multiplicative constant to convert from mkm to meter.
    let mkmToMeter: double<meter/mkm> = 1.0e-6<meter/mkm>

    /// Multiplicative constant to convert from mm to meter.
    let mmToMeter: double<meter/mm> = 1.0e-3<meter/mm>


    // let toNanometers w = w / nm
    let cplxI = Complex.create 0.0 1.0


    /// A magnitude below which a value is treated as exact zero.
    type ZeroTolerance =
        | ZeroTolerance of double

        member this.value = let (ZeroTolerance v) = this in v


    /// Treat positive numbers less than that as exact zero.
    let almostZero = ZeroTolerance 1.0e-012


    /// Treat determinant less than that as exact zero.
    let almostZeroDet = ZeroTolerance 1.0e-03
