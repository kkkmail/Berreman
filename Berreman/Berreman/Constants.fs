﻿namespace Berreman
module Constants =
    open MathNetNumericsMath

    let mm = 1.0e-03
    let mkm = 1.0e-06
    let nm = 1.0e-09
    let toNanometers w = w / nm
    let cplxI = createComplex 0.0 1.0


    /// Treat positive numbers less than that as exact zero.
    let almostZero = 1.0e-012


    /// Treat determinant less than that as exact zero.
    let almostZeroDet = 1.0e-03

