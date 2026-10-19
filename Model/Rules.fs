
module DrizzleCarton.Model.Rules

open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files

let totalUsedBytes (entries: Entry list) : int = entries |> List.sumBy Entry.size

let remainingBytes (quota: Quota) (usedBytes: int) : int = Quota.value quota - usedBytes

let fitsWithinQuota (quota: Quota) (usedBytes: int) (file: FileSize) : bool =
    FileSize.value file <= remainingBytes quota usedBytes

let entriesWithPath (tree: EntryTree) : (Entry * EntryName list) list =
    let rec walk (prefix: EntryName list) (subtree: EntryTree) =
        let current = EntryTree.entry subtree
        let path = prefix @ [ Entry.name current ]

        match subtree with
        | FileNode _ -> [ current, path ]
        | FolderNode(_, children) -> (current, path) :: List.collect (walk path) children

    walk [] tree


let largestWithPath (tree: EntryTree) : (Entry * EntryName list) option =
    match entriesWithPath tree with
    | [] -> None
    | entries -> entries |> List.maxBy (fst >> Entry.size) |> Some

let depthOf (target: EntryId) (tree: EntryTree) : Result<Depth, string> =
    match entriesWithPath tree |> List.tryFind (fst >> Entry.id >> (=) target) with
    | None -> Error "Entry does not belong to this user"
    | Some(_, path) -> Depth.create (List.length path - 1)


let validateNewFolder (tree: EntryTree) (parent: Entry) : Result<Depth, string> =
    match Entry.kind parent with
    | File _ -> Error "The parent of a new folder must be a folder"
    | Folder -> depthOf (Entry.id parent) tree |> Result.bind Depth.increment
