module DrizzleCarton.Data.Mapping

open DrizzleCarton
open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files
open DrizzleCarton.Application


let sequence (results: Result<'a, 'e> list) : Result<'a list, 'e> =
    (Ok [], results)
    ||> List.fold (fun state result ->
        match state, result with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok acc, Ok value -> Ok(value :: acc))
    |> Result.map List.rev


let ofDatabaseError (error: Database.DatabaseError) : PersistenceError =
    match error with
    | Database.NotFound _ -> RowNotFound

let toUser ((id, username, quota, rootFolder): UserTuple) : Result<User, PersistenceError> =
    match Username.create username, Quota.create quota with
    | Ok name, Ok limit ->
        Ok
            { Id = UserId.ofInt id
              Username = name
              Quota = limit
              RootFolder = EntryId.ofInt rootFolder }
    | Error reason, _
    | _, Error reason -> Error(InvalidRow $"user {id}: {reason}")

let toEntry ((id, name, parent, kind, size): EntryTuple) : Result<Entry, PersistenceError> =
    EntryName.create name
    |> Result.bind (fun entryName ->
        EntryKind.create kind size
        |> Result.bind (fun entryKind ->
            Entry.create (EntryId.ofInt id) entryName (Option.map EntryId.ofInt parent) entryKind))
    |> Result.mapError (fun reason -> InvalidRow $"entry {id}: {reason}")
