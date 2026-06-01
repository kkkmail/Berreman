/// Gated `constructor-unit-tests` host (R-6). Slice 001 ships one trivial
/// passing test so the gate (`exit_code: 0`) runs green from the first round;
/// every later OpticalConstructor.* test is hosted here, not in a separate
/// project (reconciler round-2 §g).
module OpticalConstructor.Tests.ScaffoldTests

open Xunit

[<Fact>]
let ``scaffold solution builds and the test host runs`` () =
    Assert.True(true)
