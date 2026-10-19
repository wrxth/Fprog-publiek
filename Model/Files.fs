module DrizzleCarton.Model.Files

open DrizzleCarton.Model.Ids

type EntryName = private EntryName of string

module EntryName =
    let private maxLength = 64

    let create (s: string) : Result<EntryName, string> =
        if s.Length = 0 then
            Error "Entry name may not be empty"
        elif s.Length > maxLength then
            Error $"Entry name may not be longer than {maxLength} characters, got {s.Length}"
        elif s.Contains "/" || s.Contains "\\" then
            Error "Entry name may not contain forward slashes or backslashes"
        else
            Ok(EntryName s)

    let root = EntryName "files"

    let value (EntryName s) : string = s

// FileSize

type FileSize = private FileSize of int

module FileSize =
    let create (n: int) : Result<FileSize, string> =
        if n > 0 then
            Ok(FileSize n)
        else
            Error $"A file must be larger than 0 bytes, got {n}"

    let value (FileSize n) : int = n

// EntryKind

type EntryKind =
    | Folder
    | File of FileSize

module EntryKind =
    let toString (kind: EntryKind) : string =
        match kind with
        | Folder -> "folder"
        | File _ -> "file"

    let size (kind: EntryKind) : int =
        match kind with
        | Folder -> 0
        | File bytes -> FileSize.value bytes


    let create (kind: string) (size: int) : Result<EntryKind, string> =
        match kind with
        | "folder" when size = 0 -> Ok Folder
        | "folder" -> Error $"A folder must have a size of 0, got {size}"
        | "file" -> FileSize.create size |> Result.map File
        | other -> Error $"Unknown entry kind: '{other}'"

// Entry

type Entry =
    private
        { Id: EntryId
          Name: EntryName
          Parent: EntryId option
          Kind: EntryKind }

module Entry =
    let root (id: EntryId) : Entry =
        { Id = id
          Name = EntryName.root
          Parent = None
          Kind = Folder }

    let child (id: EntryId) (name: EntryName) (parent: EntryId) (kind: EntryKind) : Entry =
        { Id = id
          Name = name
          Parent = Some parent
          Kind = kind }

    let create (id: EntryId) (name: EntryName) (parent: EntryId option) (kind: EntryKind) : Result<Entry, string> =
        match parent, kind with
        | Some parentId, _ -> Ok(child id name parentId kind)
        | None, Folder when name = EntryName.root -> Ok(root id)
        | None, _ -> Error "Only a root folder named 'files' may have no parent"

    let id (entry: Entry) : EntryId = entry.Id

    let name (entry: Entry) : EntryName = entry.Name

    let parent (entry: Entry) : EntryId option = entry.Parent

    let kind (entry: Entry) : EntryKind = entry.Kind

    let size (entry: Entry) : int = EntryKind.size entry.Kind

// Depth


type Depth = private Depth of int

module Depth =
    let maxLevels = 6

    let root = Depth 0

    let value (Depth n) : int = n

    let create (n: int) : Result<Depth, string> =
        if n >= 0 && n <= maxLevels then
            Ok(Depth n)
        else
            Error $"Entries may be nested at most {maxLevels} levels deep, got {n}"

    let increment (Depth n) : Result<Depth, string> = create (n + 1)

// EntryTree


type EntryTree =
    | FileNode of Entry
    | FolderNode of Entry * EntryTree list

module EntryTree =
    let entry (tree: EntryTree) : Entry =
        match tree with
        | FileNode e -> e
        | FolderNode(e, _) -> e

    let rec toList (tree: EntryTree) : Entry list =
        match tree with
        | FileNode e -> [ e ]
        | FolderNode(e, children) -> e :: List.collect toList children

    let tryFind (target: EntryId) (tree: EntryTree) : Entry option =
        toList tree |> List.tryFind (fun e -> Entry.id e = target)
