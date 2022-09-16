namespace Ploeh.Katas

type Endpoint<'a> = Open of 'a | Closed of 'a

module Endpoint =
    let map f = function
        | Open   x -> Open   (f x)
        | Closed x -> Closed (f x)

type Range<'a> = { LowerBound : Endpoint<'a>; UpperBound : Endpoint<'a> }

module Range =
    let ofEndpoints (lowerBound, upperBound) =
        { LowerBound = lowerBound; UpperBound = upperBound }

    let map f { LowerBound = lowerBound; UpperBound = upperBound } =
       { LowerBound = Endpoint.map f lowerBound
         UpperBound = Endpoint.map f upperBound }

    let contains ys endpoints =
        match endpoints with
        | Open x, Open z ->
            ys |> List.forall (fun y -> x  < y && y  < z)
        | Open x, Closed z ->
            ys |> List.forall (fun y -> x  < y && y <= z)
        | Closed x, Open z ->
            ys |> List.forall (fun y -> x <= y && y  < z)
        | Closed x, Closed z ->
            ys |> List.forall (fun y -> x <= y && y <= z)