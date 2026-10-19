namespace DrizzleCarton.Application

open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files


type PersistenceError =
    | RowNotFound
    | InvalidRow of reason: string

type ApplicationError =
    | UserNotFound of UserId
    | EntryNotFound of EntryId
    | InvalidRequest of reason: string
    | StorageUnavailable of reason: string

[<RequireQualifiedAccess>]
module ApplicationError =
    let ofPersistence (notFound: ApplicationError) (error: PersistenceError) : ApplicationError =
        match error with
        | RowNotFound -> notFound
        | InvalidRow reason -> StorageUnavailable reason


type IUserRepository =
    abstract member All: unit -> Result<User list, PersistenceError>
    abstract member ById: UserId -> Result<User, PersistenceError>
    abstract member Add: Username * Quota * EntryId -> Result<User, PersistenceError>
    abstract member Update: User -> Result<User, PersistenceError>

type IEntryRepository =
    abstract member ById: EntryId -> Result<Entry, PersistenceError>
    abstract member TreeFrom: EntryId -> Result<EntryTree, PersistenceError>
    abstract member AddRoot: unit -> Result<Entry, PersistenceError>
    abstract member AddFolder: EntryName * EntryId -> Result<Entry, PersistenceError>
