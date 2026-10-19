module DrizzleCarton.Data.Users

open DrizzleCarton
open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Application


type UserRepository(db: Database) =

    interface IUserRepository with

        member _.All() =
            Database.users db
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind (List.map Mapping.toUser >> Mapping.sequence)

        member _.ById id =
            Database.user (UserId.value id) db
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind Mapping.toUser

        member _.Add(name, quota, rootFolder) =
            Database.addUser db (Username.value name, Quota.value quota, EntryId.value rootFolder)
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind Mapping.toUser

        member _.Update user =
            Database.updateUser
                db
                (UserId.value user.Id,
                 Username.value user.Username,
                 Quota.value user.Quota,
                 EntryId.value user.RootFolder)
            |> Result.mapError Mapping.ofDatabaseError
            |> Result.bind Mapping.toUser
