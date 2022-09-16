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

[<Fact>]
let ``Open range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.int32 (Range.linearBounded ())
    let! max = Gen.int32 (Range.linearBounded ())

    let actual = (Open min, Open max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range (%i, %i) expected not to contain list." min max) }

[<Fact>]
let ``Open range contains list`` () = Property.check <| property {
    let! xs = Gen.int64 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs - 1L
    let max = List.max xs + 1L

    let actual = (Open min, Open max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range (%i, %i) expected to contain list." min max) }

[<Fact>]
let ``Open-closed range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.int16 (Range.linearBounded ())
    let! max = Gen.int16 (Range.linearBounded ())

    let actual = (Open min, Closed max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range (%i, %i] expected not to contain list." min max) }

[<Fact>]
let ``Open-closed range contains list`` () = Property.check <| property {
    let! xs = Gen.int64 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs - 1L
    let max = List.max xs

    let actual = (Open min, Closed max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range (%i, %i] expected to contain list." min max) }

[<Fact>]
let ``Closed-open range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.byte (Range.linearBounded ())
    let! max = Gen.byte (Range.linearBounded ())

    let actual = (Closed min, Open max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range [%i, %i) expected not to contain list." min max) }
