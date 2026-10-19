module DrizzleCarton.Data.Files

open DrizzleCarton
open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Files
open DrizzleCarton.Application

let private readEntry (db: Database) (id: EntryId) : Result<Entry, PersistenceError> =
    Database.entry db (EntryId.value id)
    |> Result.mapError Mapping.ofDatabaseError
    |> Result.bind Mapping.toEntry

let rec private readTree (db: Database) (id: EntryId) : Result<EntryTree, PersistenceError> =
    readEntry db id
    |> Result.bind (fun entry ->
        match Entry.kind entry with
        | File _ -> Ok(FileNode entry)
        | Folder ->
            Database.subEntries db (EntryId.value id)
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind (fun rows ->
                rows
                |> List.map (fun (childId, _, _, _, _) -> readTree db (EntryId.ofInt childId))
                |> Mapping.sequence)
            |> Result.map (fun children -> FolderNode(entry, children)))

type EntryRepository(db: Database) =

    interface IEntryRepository with

        member _.ById id = readEntry db id

        member _.TreeFrom id = readTree db id

        member _.AddRoot() =
            Database.addEntry db (EntryName.value EntryName.root, None, EntryKind.toString Folder, 0)
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind Mapping.toEntry

        member _.AddFolder(name, parent) =
            Database.addEntry
                db
                (EntryName.value name, Some(EntryId.value parent), EntryKind.toString Folder, 0)
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind Mapping.toEntry
