module DrizzleCarton.Application.Files

open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files
open DrizzleCarton.Model
open DrizzleCarton.Application

module UserCases = DrizzleCarton.Application.Users

type QuotaUsage =
    { Used: int
      Quota: Quota
      Remaining: int }

type LargestEntry = { Entry: Entry; Path: EntryName list }

let private userWithTree
    (users: IUserRepository)
    (entries: IEntryRepository)
    (id: UserId)
    : Result<User * EntryTree, ApplicationError> =
    UserCases.byId users id
    |> Result.bind (fun user ->
        entries.TreeFrom user.RootFolder
        |> Result.mapError (ApplicationError.ofPersistence (EntryNotFound user.RootFolder))
        |> Result.map (fun tree -> user, tree))

let tree (users: IUserRepository) (entries: IEntryRepository) (id: UserId) : Result<EntryTree, ApplicationError> =
    userWithTree users entries id |> Result.map snd

/// Use case 1: een nieuwe map maken in de root folder van een gebruiker of in
let createFolder
    (users: IUserRepository)
    (entries: IEntryRepository)
    (id: UserId)
    (parentId: EntryId)
    (name: EntryName)
    : Result<Entry, ApplicationError> =
    userWithTree users entries id
    |> Result.bind (fun (_, tree) ->
        match EntryTree.tryFind parentId tree with
        | None -> Error(EntryNotFound parentId)
        | Some parent ->
            Rules.validateNewFolder tree parent
            |> Result.mapError InvalidRequest
            |> Result.bind (fun _ ->
                entries.AddFolder(name, parentId)
                |> Result.mapError (ApplicationError.ofPersistence (EntryNotFound parentId))))

/// Use case 2: het totaal aantal bytes dat een gebruiker heeft opgeslagen.
let quotaUsage
    (users: IUserRepository)
    (entries: IEntryRepository)
    (id: UserId)
    : Result<QuotaUsage, ApplicationError> =
    userWithTree users entries id
    |> Result.map (fun (user, tree) ->
        let used = tree |> EntryTree.toList |> Rules.totalUsedBytes

        { Used = used
          Quota = user.Quota
          Remaining = Rules.remainingBytes user.Quota used })

/// Use case 3: het grootste bestand van een gebruiker, inclusief zijn pad.
let largest
    (users: IUserRepository)
    (entries: IEntryRepository)
    (id: UserId)
    : Result<LargestEntry, ApplicationError> =
    userWithTree users entries id
    |> Result.bind (fun (_, tree) ->
        match Rules.largestWithPath tree with
        | None -> Error(InvalidRequest "this user has no entries")
        | Some(entry, path) -> Ok { Entry = entry; Path = path })
