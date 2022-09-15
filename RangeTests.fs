module Ploeh.Katas.RangeTests

open Xunit
open Hedgehog

[<Fact>]
let ``Closed range contains list`` () = Property.check <| property {
    let! xs = Gen.int16 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs
    let max = List.max xs

    let actual = (Closed min, Closed max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range [%i, %i] expected to contain list." min max) }