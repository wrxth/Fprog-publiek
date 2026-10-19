
module DrizzleCarton.Service.Users

open Giraffe
open Microsoft.AspNetCore.Http
open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Application
open DrizzleCarton.Service.Serialization

module UseCases = DrizzleCarton.Application.Users

type UserDto =
    { Id: int
      Username: string
      Quota: int
      RootFolder: int }

type CreateUserDto = { Name: string; Quota: int }

let private toDto (user: User) : UserDto =
    { Id = UserId.value user.Id
      Username = Username.value user.Username
      Quota = Quota.value user.Quota
      RootFolder = EntryId.value user.RootFolder }

let private userRepository (ctx: HttpContext) = ctx.GetService<IUserRepository>()

let private entryRepository (ctx: HttpContext) = ctx.GetService<IEntryRepository>()

let all: HttpHandler =
    fun next ctx -> respond (List.map toDto) (UseCases.all (userRepository ctx)) next ctx

let byId (id: int) : HttpHandler =
    fun next ctx -> respond toDto (UseCases.byId (userRepository ctx) (UserId.ofInt id)) next ctx

let rename (id: int) : HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyBufferedFromRequestAsync()

            let result =
                decodeBody<string> body
                |> Result.bind (Username.create >> Result.mapError InvalidRequest)
                |> Result.bind (UseCases.rename (userRepository ctx) (UserId.ofInt id))

            return! respond toDto result next ctx
        }

let create: HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyBufferedFromRequestAsync()
            let result =
                decodeBody<CreateUserDto> body
                |> Result.bind (fun dto ->
                    match Username.create dto.Name, Quota.create dto.Quota with
                    | Ok name, Ok quota -> Ok(name, quota)
                    | Error reason, _
                    | _, Error reason -> Error(InvalidRequest reason))
                |> Result.bind (fun (name, quota) ->
                    UseCases.create (userRepository ctx) (entryRepository ctx) name quota)

            return! respond toDto result next ctx
        }
