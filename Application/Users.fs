module DrizzleCarton.Application.Users

open DrizzleCarton.Model.Ids
open DrizzleCarton.Model.Users
open DrizzleCarton.Model.Files
open DrizzleCarton.Application

let all (users: IUserRepository) : Result<User list, ApplicationError> =
    users.All()
    |> Result.mapError (ApplicationError.ofPersistence (StorageUnavailable "the user table is missing"))

let byId (users: IUserRepository) (id: UserId) : Result<User, ApplicationError> =
    users.ById id |> Result.mapError (ApplicationError.ofPersistence (UserNotFound id))


let rename (users: IUserRepository) (id: UserId) (name: Username) : Result<User, ApplicationError> =
    byId users id
    |> Result.map (User.rename name)
    |> Result.bind (fun renamed ->
        users.Update renamed
        |> Result.mapError (ApplicationError.ofPersistence (UserNotFound id)))


let create
    (users: IUserRepository)
    (entries: IEntryRepository)
    (name: Username)
    (quota: Quota)
    : Result<User, ApplicationError> =
    entries.AddRoot()
    |> Result.mapError (ApplicationError.ofPersistence (StorageUnavailable "could not create a root folder"))
    |> Result.bind (fun root ->
        users.Add(name, quota, Entry.id root)
        |> Result.mapError (ApplicationError.ofPersistence (StorageUnavailable "could not create the user")))
