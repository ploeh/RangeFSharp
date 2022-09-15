namespace Ploeh.Katas

type Endpoint<'a> = Open of 'a | Closed of 'a

module Range =
    let contains _ _ = true