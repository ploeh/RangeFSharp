module Ploeh.Katas.EndpointTests

open Xunit
open Hedgehog
open Swensen.Unquote

[<Fact>]
let ``First functor law`` () = Property.check <| property {
    let genInt32 = Gen.int32 (Range.linearBounded ())
    let! expected = Gen.choice [Gen.map Open genInt32; Gen.map Closed genInt32]

    let actual = Endpoint.map id expected

    expected =! actual }

[<Fact>]
let ``Second functor law`` () = Property.check <| property {
    let genInt32 = Gen.int32 (Range.linearBounded ())
    let! endpoint = Gen.choice [Gen.map Open genInt32; Gen.map Closed genInt32]
    let! f = Gen.item [id; ((+) 1); ((*) 2)]
    let! g = Gen.item [id; ((+) 1); ((*) 2)]

    let actual = Endpoint.map (f << g) endpoint

    Endpoint.map f (Endpoint.map g endpoint) =! actual }