module DrizzleCarton.Model.Ids

type UserId = private UserId of int

type EntryId = private EntryId of int

module UserId =
    let ofInt (n: int) : UserId = UserId n

    let value (UserId n) : int = n

module EntryId =
    let ofInt (n: int) : EntryId = EntryId n

    let value (EntryId n) : int = n
