module Ploeh.Katas.RangeTests

open Xunit
open Hedgehog
open Swensen.Unquote

[<Fact>]
let ``Closed range contains list`` () = Property.check <| property {
    let! xs = Gen.int16 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs
    let max = List.max xs

    let actual = Range.ofEndpoints (Closed min, Closed max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range [%i, %i] expected to contain list." min max) }

[<Fact>]
let ``Open range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.int32 (Range.linearBounded ())
    let! max = Gen.int32 (Range.linearBounded ())

    let actual = Range.ofEndpoints (Open min, Open max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range (%i, %i) expected not to contain list." min max) }

[<Fact>]
let ``Open range contains list`` () = Property.check <| property {
    let! xs = Gen.int64 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs - 1L
    let max = List.max xs + 1L

    let actual = Range.ofEndpoints (Open min, Open max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range (%i, %i) expected to contain list." min max) }

[<Fact>]
let ``Open-closed range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.int16 (Range.linearBounded ())
    let! max = Gen.int16 (Range.linearBounded ())

    let actual = Range.ofEndpoints (Open min, Closed max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range (%i, %i] expected not to contain list." min max) }

[<Fact>]
let ``Open-closed range contains list`` () = Property.check <| property {
    let! xs = Gen.int64 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs - 1L
    let max = List.max xs

    let actual = Range.ofEndpoints (Open min, Closed max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range (%i, %i] expected to contain list." min max) }

[<Fact>]
let ``Closed-open range doesn't contain endpoints`` () = Property.check <| property {
    let! min = Gen.byte (Range.linearBounded ())
    let! max = Gen.byte (Range.linearBounded ())

    let actual = Range.ofEndpoints (Closed min, Open max) |> Range.contains [min; max]

    Assert.False
        (actual, sprintf "Range [%i, %i) expected not to contain list." min max) }

[<Fact>]
let ``Closed-open range contains list`` () = Property.check <| property {
    let! xs = Gen.int32 (Range.linearBounded ()) |> Gen.list (Range.linear 1 99)
    let min = List.min xs
    let max = List.max xs + 1

    let actual = Range.ofEndpoints (Closed min, Open max) |> Range.contains xs

    Assert.True
        (actual, sprintf "Range [%i, %i) expected to contain list." min max) }

[<Fact>]
let ``Closed range doesn't contain points outside range`` () = Property.check <| property {
    let! min = Gen.int32 (Range.linearBounded ())
    let! size = Gen.int32 (Range.linear 1 99)
    let max = min + size

    let outside = min - 1
    let actual = Range.ofEndpoints (Closed min, Closed max) |> Range.contains [outside]

    Assert.False (
        actual,
        sprintf "Range [%i, %i] expected not to contain [%i]." min max outside) }

[<Fact>]
let ``First functor law`` () = Property.check <| property {
    let genInt64 = Gen.int64 (Range.linearBounded ())
    let genEndpoint = Gen.choice [Gen.map Open genInt64; Gen.map Closed genInt64]
    let! expected = Gen.tuple genEndpoint |> Gen.map Range.ofEndpoints

    let actual = expected |> Ploeh.Katas.Range.map id

    expected =! actual }

[<Fact>]
let ``Second functor law`` () = Property.check <| property {
    let genInt16 = Gen.int16 (Range.linearBounded ())
    let genEndpoint = Gen.choice [Gen.map Open genInt16; Gen.map Closed genInt16]
    let! range = Gen.tuple genEndpoint |> Gen.map Range.ofEndpoints
    let! f = Gen.item [id; ((+) 1s); ((*) 2s)]
    let! g = Gen.item [id; ((+) 1s); ((*) 2s)]

    let actual = range |> Ploeh.Katas.Range.map (f << g)

    Ploeh.Katas.Range.map f (Ploeh.Katas.Range.map g range) =! actual }