namespace Ploeh.Katas

type Endpoint<'a> = Open of 'a | Closed of 'a

module Range =
    let contains _ endpoints =
        match endpoints with
        | Open _, Open _ -> false
        | _ -> true