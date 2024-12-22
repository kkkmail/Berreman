namespace Berreman
module Constants =
    open MathNetNumericsMath

    [<Measure>]
    type meter


    [<Measure>]
    type mm


    [<Measure>]
    type mkm


    [<Measure>]
    type nm


    let oneNanometer = 1.0<nm>
    let oneMicrometer = 1.0<mkm>
    let oneMillimeter = 1.0<mm>


    let nmPerMeter : double<meter/nm> = 1.0e9<meter/nm>
    let mkmPerMeter : double<meter/mkm> = 1.0e6<meter/mkm>
    let mmPerMeter : double<meter/mm> = 1.0e3<meter/mm>


    // let toNanometers w = w / nm
    let cplxI = createComplex 0.0 1.0


    /// Treat positive numbers less than that as exact zero.
    let almostZero = 1.0e-012


    /// Treat determinant less than that as exact zero.
    let almostZeroDet = 1.0e-03
