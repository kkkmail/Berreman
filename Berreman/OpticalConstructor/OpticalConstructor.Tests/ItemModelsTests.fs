namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.ItemModels

/// The parameter vocabulary moved to `ItemModels` unchanged (spec 0050 S7.1).
module ItemModelsTests =

    [<Fact>]
    let ``the moved parameter name and dial sign keep their values`` () : unit =
        Assert.Equal("c2.retardance", (ParameterName "c2.retardance").value)
        Assert.Equal<float>(1.0, DialPositive.value)
        Assert.Equal<float>(-1.0, DialNegative.value)
