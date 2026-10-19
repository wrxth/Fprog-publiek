
module DrizzleCarton.Service.Serialization

open Giraffe
open Thoth.Json.Net
open DrizzleCarton.Model.Ids
open DrizzleCarton.Application

let json<'T> (value: 'T) : HttpHandler =
    setHttpHeader "Content-Type" "application/json"
    >=> setBodyFromString (Encode.Auto.toString (4, value))

let fromJson<'T> = Decode.Auto.fromString<'T>


let private toHttpError (error: ApplicationError) : HttpHandler =
    match error with
    | UserNotFound id -> RequestErrors.NOT_FOUND $"There is no user with id {UserId.value id}"
    | EntryNotFound id -> RequestErrors.NOT_FOUND $"There is no entry with id {EntryId.value id}"
    | InvalidRequest reason -> RequestErrors.BAD_REQUEST reason
    | StorageUnavailable reason -> ServerErrors.INTERNAL_ERROR reason


let respond (toDto: 'a -> 'b) (result: Result<'a, ApplicationError>) : HttpHandler =
    match result with
    | Ok value -> json (toDto value)
    | Error error -> toHttpError error


let decodeBody<'T> (body: string) : Result<'T, ApplicationError> =
    fromJson<'T> body |> Result.mapError InvalidRequest
