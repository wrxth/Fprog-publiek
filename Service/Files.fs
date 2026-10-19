module DrizzleCarton.Service.Files

open Giraffe
open Microsoft.AspNetCore.Http
open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files
open DrizzleCarton.Application
open DrizzleCarton.Service.Serialization

module UseCases = DrizzleCarton.Application.Files

type EntryDto =
    { Id: int
      Name: string
      Parent: int option
      Kind: string
      Size: int }

type TreeDto =
    { Entry: EntryDto
      Subentries: TreeDto list }

type QuotaDto =
    { UsedBytes: int
      QuotaBytes: int
      RemainingBytes: int }

type LargestEntryDto = { Entry: EntryDto; Path: string }

type CreateFolderDto = { ParentId: int; Name: string }

let private toEntryDto (entry: Entry) : EntryDto =
    { Id = EntryId.value (Entry.id entry)
      Name = EntryName.value (Entry.name entry)
      Parent = Entry.parent entry |> Option.map EntryId.value
      Kind = EntryKind.toString (Entry.kind entry)
      Size = Entry.size entry }

let rec private toTreeDto (tree: EntryTree) : TreeDto =
    match tree with
    | FileNode entry ->
        { Entry = toEntryDto entry
          Subentries = [] }
    | FolderNode(entry, children) ->
        { Entry = toEntryDto entry
          Subentries = children |> List.map toTreeDto }

let private toQuotaDto (usage: UseCases.QuotaUsage) : QuotaDto =
    { UsedBytes = usage.Used
      QuotaBytes = Quota.value usage.Quota
      RemainingBytes = usage.Remaining }

let private toLargestDto (largest: UseCases.LargestEntry) : LargestEntryDto =
    { Entry = toEntryDto largest.Entry
      Path = largest.Path |> List.map EntryName.value |> String.concat "/" }

let private repositories (ctx: HttpContext) =
    ctx.GetService<IUserRepository>(), ctx.GetService<IEntryRepository>()

let tree (userId: int) : HttpHandler =
    fun next ctx ->
        let users, entries = repositories ctx
        respond toTreeDto (UseCases.tree users entries (UserId.ofInt userId)) next ctx

let quota (userId: int) : HttpHandler =
    fun next ctx ->
        let users, entries = repositories ctx
        respond toQuotaDto (UseCases.quotaUsage users entries (UserId.ofInt userId)) next ctx

let largest (userId: int) : HttpHandler =
    fun next ctx ->
        let users, entries = repositories ctx
        respond toLargestDto (UseCases.largest users entries (UserId.ofInt userId)) next ctx

let createFolder (userId: int) : HttpHandler =
    fun next ctx ->
        task {
            let users, entries = repositories ctx
            let! body = ctx.ReadBodyBufferedFromRequestAsync()

            let result =
                decodeBody<CreateFolderDto> body
                |> Result.bind (fun dto ->
                    EntryName.create dto.Name
                    |> Result.mapError InvalidRequest
                    |> Result.map (fun name -> EntryId.ofInt dto.ParentId, name))
                |> Result.bind (fun (parentId, name) ->
                    UseCases.createFolder users entries (UserId.ofInt userId) parentId name)

            return! respond toEntryDto result next ctx
        }
