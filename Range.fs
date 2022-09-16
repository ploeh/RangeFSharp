namespace Ploeh.Katas

type Endpoint<'a> = Open of 'a | Closed of 'a

module Range =
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